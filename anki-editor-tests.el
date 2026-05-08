;;; anki-editor-tests.el --- Tests for anki-editor -*- lexical-binding: t; -*-

;; URL: https://github.com/anki-editor/anki-editor
;; Package-Requires: ((emacs "25.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'anki-editor)
(require 'org-attach)

(defun anki-editor-test--org-export-get-reference (_datum _info)
  "Return reference. DATUM and INFO are ignored."
  "test")

(defvar anki-editor-test-python-server nil
  "Python server process for testing.")

(defun anki-editor-test-start-python-server ()
  "Start python server for testing."
  (interactive)

  (let ((default-directory (file-name-directory (symbol-file 'anki-editor-test-start-python-server))))
    (message "Starting python server for testing...")
    (setq anki-editor-test-python-server
          (start-process-shell-command "anki-editor-test--start-python-server" "*anki-editor-test--start-python-server*" "python3 test_server.py"))
    (sleep-for 1)
    (message "Python server started.")))

(defun anki-editor-test-stop-python-server ()
  "Stop python server for testing."
  (interactive)

  ;; Check if server is running
  (when (process-live-p anki-editor-test-python-server)
    (message "Stopping python server for testing...")
    (signal-process "anki-editor-test--start-python-server" 'term)
    (cl-loop with retries-left = 100
             until (or (not (process-live-p anki-editor-test-python-server))
                       (zerop retries-left))
             do (sleep-for 0.1)
             do (setq retries-left (1- retries-left)))

    (when (process-live-p anki-editor-test-python-server)
      (kill-process anki-editor-test-python-server))
    (message "Python server stopped.")))

(defun anki-editor-test-restart-python-server ()
  "Stop python server for testing."
  (interactive)
  (anki-editor-test-stop-python-server)
  ;; Wait for process to stop, limiting the wait time by 5 seconds
  (anki-editor-test-start-python-server))

(defun anki-editor-test-ensure-python-server ()
  "Ensure python server is running."
  (unless (process-live-p anki-editor-test-python-server)
    (anki-editor-test-start-python-server)))

(defvar anki-editor-test--saved-variable-values nil
  "Association list of saved variable values.")

(defun anki-editor-test--patch-variables (variables)
  "Update VARIABLES, saving their original values.

Origian values are stored in
`anki-editor-test--saved-variable-values'.

You can restore the original values by calling
`anki-editor-test--restore-variables'."
  (cl-loop for (variable . value) in variables
           when (boundp variable)
           do (push (cons variable (symbol-value variable)) anki-editor-test--saved-variable-values)
           do (set variable value)))

(defun anki-editor-test--restore-variables ()
  "Restore variables to their original values."
  (cl-loop for (variable . value) in anki-editor-test--saved-variable-values
           do (set variable value))
  (setq anki-editor-test--saved-variable-values nil))

(defun anki-editor-test--setup ()
  "Setup testing."
  (anki-editor-test--patch-variables
   '((anki-editor-api-port . 28765)
     (anki-editor-prepend-heading . nil)
     (org-attach-dir-relative . t)
     (anki-editor-prepend-heading-format . "test *%s*")
     (org-html-htmlize-output-type . nil)
     (org-html-link-use-abs-url . nil)))
  (advice-add 'org-export-get-reference :override #'anki-editor-test--org-export-get-reference)
  (anki-editor-test-ensure-python-server)
  (anki-editor-mode 1))

(defun anki-editor-test--teardown ()
  "Teardown testing."
  (anki-editor-test--restore-variables)
  (advice-remove 'org-export-get-reference #'anki-editor-test--org-export-get-reference))

(defun anki-editor-test--go-to-headline (title)
  "Go to headline with TITLE."
  (goto-char (point-min))
  (re-search-forward (concat "* " title)))

(defun anki-editor-test--test-org-buffer (name)
  "Return test org buffer with NAME."
  (find-file-noselect (expand-file-name name (file-name-directory (symbol-file 'anki-editor-test--go-to-headline)))))

(cl-defmacro anki-editor-deftest (name () &key doc in test)
  "Define NAME as an `anki-editor' specific `ert' test.
This sets up the test server, runs the test, and then tears everything
down again.  The DOC key is an (optional) doc string, IN is the file
that the test should run in (containing the notes), and TEST is the
actual body of the test."
  (declare (doc-string 3) (indent 2))
  `(ert-deftest ,name ()
     ,doc
     (save-window-excursion
       (with-current-buffer (anki-editor-test--test-org-buffer ,in)
         (anki-editor-test--setup)
         (unwind-protect ,test
           (anki-editor-test--teardown))))))

(anki-editor-deftest test--export-string-with-raw ()
  :doc "Test `anki-editor--export-string` with `# raw` prefix."
  :in "test-files/test.org"
  :test
  (progn
    (should (equal (anki-editor--export-string "# raw content") "content"))
    (should (equal (anki-editor--export-string "# raw  content") "content"))
    (should (equal (anki-editor--export-string "# raw\ncontent") "content"))
    (should (equal (anki-editor--export-string "# raw") ""))))

(anki-editor-deftest test--export-string-without-raw ()
  :doc "Test `anki-editor--export-string` without `# raw` prefix."
  :in "test-files/test.org"
  :test
  (progn
    (should (equal (anki-editor--export-string "content") "<p>\ncontent</p>\n"))
    (should (equal (anki-editor--export-string "") ""))))

(ert-deftest test--concat-fields-should-concatenate-fields-into-string ()
  "Test `anki-editor--concat-fields' should concatenate fields into string."
  (should (equal (anki-editor--concat-fields '("Front" "Back")
                                             '(("Front" . "Front content")
                                               ("Back" . "Back content"))
                                             0)
                 "* Front

Front content

* Back

Back content

")))

(ert-deftest test--concat-fields-when-field-name-missing-in-field-alist-should-ignore-it ()
  "Test `anki-editor--concat-fields' should ignore field name missing in field-alist."
  (let ((concat-out (anki-editor--concat-fields '("Front" "Back")
                                                '(("Front" . "Front content")) 0)))
    (should (stringp concat-out))
    (should (equal concat-out
                   "* Front

Front content

"))))

(anki-editor-deftest test--note-at-point-should-return-note-at-point ()
  :doc "Test `anki-editor--note-at-point' should return note at point."
  :in "test-files/test.org"
  :test
  (let ((expected-note
         #s(anki-editor-note nil "Basic" "Tests"
                             (("Back" . "<p>
Lorem
</p>
")
                              ("Front" . "<p>
Simple note body
</p>
")) nil nil nil))
        (note-at-point (progn (goto-char (point-min)) (anki-editor-note-at-point))))
    (setf (anki-editor-note-fields note-at-point)
          (anki-editor--export-fields (anki-editor-note-fields note-at-point)))
    (setf (anki-editor-note-marker expected-note) (point-marker))
    (should (equal expected-note note-at-point))))

(anki-editor-deftest test--note-at-point-for-note-with-property-field-should-render-property-field ()
  :doc "Test `anki-editor--note-at-point' should render property field."
  :in "test-files/property-fields.org"
  :test
  (let ((expected-note
         #s(anki-editor-note nil "Basic" "Tests"
                             (("Back" . "<p>\nShould be included\n</p>\n")
                              ("Front" . "<p>\nCan one define an anki-field inside an org-mode property?</p>\n"))
                             nil nil nil))
        (note-at-point (progn (goto-char (point-min)) (anki-editor-note-at-point))))
    (setf (anki-editor-note-fields note-at-point)
          (anki-editor--export-fields (anki-editor-note-fields note-at-point)))
    (setf (anki-editor-note-marker expected-note) (point-marker))
    (should (equal note-at-point expected-note))))

(anki-editor-deftest test--note-at-point-for-note-with-property-field-should-override-subheading-field ()
  :doc "Test `anki-editor--note-at-point' should override subheading field."
  :in "test-files/property-fields.org"
  :test
  (let ((expected-note #s(anki-editor-note nil "Basic" "Tests"
                                           (("Back" . "<p>\nShould be included\n</p>\n")
                                            ("Front" . "<p>\nCan one define an anki-field inside an org-mode property?</p>\n"))
                                           nil nil nil))
        (note-at-point (progn (goto-char (point-min)) (anki-editor-note-at-point))))
    (setf (anki-editor-note-fields note-at-point)
          (anki-editor--export-fields (anki-editor-note-fields note-at-point)))
    (setf (anki-editor-note-marker expected-note) (point-marker))
    (should (equal note-at-point expected-note))))

(anki-editor-deftest test--note-at-point-for-examples-should-produce-correct-output ()
  :doc "Test `anki-editor--note-at-point' should produce correct output for examples."
  :in "examples.org"
  :test
  (let ((test-items-alist
         '(("Deck in file" . #s(anki-editor-note
                                nil "Cloze" "Default"
                                (("Back Extra" . "<p>\nDeck in file</p>\n")
                                 ("Text" . "<p>\nCards of this note will be created in {{c1::Default::which deck?}}\n</p>\n"))
                                nil nil nil))
           ("Deck in entry" . #s(anki-editor-note
                                 nil "Cloze" "Languages"
                                 (("Back Extra" . "<p>\nDeck in entry</p>\n")
                                  ("Text" . "<p>\nCards of this note will be created in {{c1::Languages::which deck?}}\n</p>\n"))
                                 nil nil nil))
           ("Raw fields" . #s(anki-editor-note
                              nil "Basic" "Default"
                              (("Back" . "   With property <code>:ANKI_FORMAT: nil</code>, content of the\n   field will be sent to Anki <em>unprocessed</em>.  You can use\n   whatever Anki supports, like HTML tags.\n   <br>\n   <br>\n   This property is retrieved with inheritance, meaning that it can be\n   set in any ancestor entries or at the top of the file with\n   <code>#+PROPERTY: ANKI_FORMAT nil</code>, it's also possible to\n   override an outer level nil format with <code>:ANKI_FORMAT: t</code>.\n")
                               ("Front" . "   How to send the content of a field or fields to Anki as is?\n\n"))
                              nil nil nil))
           ("Raw and HTML-formatted fields in the same note" . #s(anki-editor-note
                                                                  nil "Basic" "Default"
                                                                  (("Back" . "If you start a field with ~# raw~, the content will be sent to Anki as is.\n")
                                                                   ("Front" . "<p>\nThis field will be formatted as HTML.\n</p>\n"))
                                                                  nil nil nil))
           ("Is there a shorter way to write notes?" . #s(anki-editor-note
                                                          nil "Basic" "Default"
                                                          (("Back" . "<p>\nYes, like this one, Front is missing, <code>anki-editor</code> will use note\nheading as Front.  This is neat as sometimes it's verbose to repeat\nthe same content in note heading and first field.\n</p>\n\n<p>\nThis works for all note types, just make one field absent and\n<code>anki-editor</code> will use note heading as that missing field.\n</p>\n")
                                                           ("Front" . "<p>\nIs there a shorter way to write notes?</p>\n"))
                                                          nil nil nil))
           ("Raining" . #s(anki-editor-note
                           nil "Basic (and reversed card)" "Languages"
                           (("Back" . "<p>\nit's raining very hard\n</p>\n")
                            ("Front" . "<p>\n(it's) raining cats and dogs\n</p>\n"))
                           ("vocab" "idioms" "english") nil nil))
           ("名词从句" . #s(anki-editor-note
                            nil "Basic" "Languages"
                            (("Back" . "<ol class=\"org-ol\">\n<li>That + 一个完整的句子, that无实际意义</li>\n<li>由疑问句改装而成</li>\n</ol>\n")
                             ("Front" . "<p>\n名词从句有哪些形式？\n</p>\n"))
                            ("grammar" "english") nil nil))
           ("Cantonese" . #s(anki-editor-note
                             nil "Basic (and reversed card)" "Languages"
                             (("Back" . "<p>\n吃过饭了没？\n</p>\n")
                              ("Front" . "<p>\n食咗饭未吖？\n</p>\n"))
                             ("cantonese" "dialect") nil nil))
           ("Emacs Lisp" . #s(anki-editor-note
                              nil "Basic" "Computing"
                              (("Back" . "<div align=\"left\">\n\n<div class=\"org-src-container\">\n<pre class=\"src src-emacs-lisp\">(condition-case the-error\n    ;; the protected form\n    (progn\n      (do-something-dangerous)\n      (do-something-more-dangerous))\n  ;; handlers\n  (error-symbol1 (handler1 the-error))\n  ((error-symbol2 error-symbol3 (handler the-error))))\n</pre>\n</div>\n\n</div>\n")
                               ("Front" . "<p>\nHow to trap errors in emacs lisp?\n</p>\n"))
                              ("lisp" "emacs" "programming") nil nil))
           ("Dot product" . #s(anki-editor-note
                               nil "Basic" "Mathematics"
                               (("Back" . "<p>\n<p>[$$]\\alpha \\cdot \\beta = a_1b_1 + a_2b_2 + a_3b_3[/$$]</p>\n</p>\n")
                                ("Front" . "<p>\nHow to calculate the dot product of two vectors:\n</p>\n\n[latex]<br>\\begin{equation*}<br>\\alpha = \\{a_1, a_2, a_3\\}, \\beta = \\{b_1, b_2, b_3\\}<br>\\end{equation*}<br>[/latex]\n"))
                               nil nil nil))
           )))
    (org-map-entries
     (lambda ()
       ;; Only entries which have ANKI_NOTE_TYPE
       (when (org-entry-get (point) "ANKI_NOTE_TYPE")
         (let ((note-at-point nil)
               (headline nil)
               (expected-note nil))
           (setq headline (org-entry-get (point) "ITEM"))
           (setq note-at-point (anki-editor-note-at-point))
           (setf (anki-editor-note-fields note-at-point)
                 (anki-editor--export-fields
                  (anki-editor-note-fields note-at-point)))
           (setq expected-note (cdr (assoc headline test-items-alist)))
           (setf (anki-editor-note-marker expected-note)
                 (point-marker))
           (should (equal note-at-point expected-note))))))))

(anki-editor-deftest test--anki-editor--map-fields-cloze-default ()
  :doc "Test `anki-editor--map-fields' should process default note."
  :in "test-files/cloze.org"
  :test
  (let* ((anki-editor-swap-two-fields nil)
         (note (progn
                 (anki-editor-test--go-to-headline "Default note")
                 (anki-editor-note-at-point)))
         (fields (anki-editor-note-fields note))
         (first-field (nth 0 fields))
         (second-field (nth 1 fields)))
    (should (and (string= "Back Extra" (car first-field))
                 (string-match "Default note" (cdr first-field))))
    (should (and (string= "Text" (car second-field))
                 (string-match "This is the {{c1::content}}." (cdr second-field))))))

(anki-editor-deftest test--anki-editor--map-fields-cloze-default-with-extra ()
  :doc "Test `anki-editor--map-fields' should process default note with extra."
  :in "test-files/cloze.org"
  :test
  (let* ((anki-editor-swap-two-fields nil)
         (note (progn
                 (anki-editor-test--go-to-headline "Default note with Extra")
                 (anki-editor-note-at-point)))
         (fields (anki-editor-note-fields note))
         (first-field (nth 0 fields))
         (second-field (nth 1 fields)))
    (should (and (string= "Back Extra" (car first-field))
                 (string-match "This is the extra content." (cdr first-field))))
    (should (and (string= "Text" (car second-field))
                 (string-match "This is the {{c1::content}}." (cdr second-field))))))

(anki-editor-deftest test--anki-editor--map-fields-cloze-should-not-swap-heading-and-content-before-subheadings ()
  :doc "Test `anki-editor--map-fields' should not swap heading and content before subheadings."
  :in "test-files/cloze.org"
  :test
  (let* ((anki-editor-swap-two-fields nil)
         (note (progn
                 (anki-editor-test--go-to-headline "Text subheading omitted")
                 (anki-editor-note-at-point)))
         (fields (anki-editor-note-fields note))
         (first-field (nth 0 fields))
         (second-field (nth 1 fields)))
    (should (and (string= "Back Extra" (car first-field))
                 (string-match "This is the {{c1::content}}." (cdr first-field))))
    (should (and (string= "Text" (car second-field))
                 (string-match "Text subheading omitted" (cdr second-field))))))

(anki-editor-deftest test--anki-editor--map-fields-cloze-should-swap-heading-and-content-before-subheadings ()
  :doc "Test `anki-editor--map-fields' should swap heading and content before subheadings."
  :in "test-files/cloze.org"
  :test
  (let* ((anki-editor-swap-two-fields '("Cloze"))
         (note (progn
                 (anki-editor-test--go-to-headline "Text subheading omitted")
                 (anki-editor-note-at-point)))
         (fields (anki-editor-note-fields note))
         (first-field (nth 0 fields))
         (second-field (nth 1 fields)))
    (should (and (string= "Back Extra" (car first-field))
                 (string-match "Text subheading omitted" (cdr first-field))))
    (should (and (string= "Text" (car second-field))
                 (string-match "This is the {{c1::content}}." (cdr second-field))))))

(anki-editor-deftest test--anki-editor-field-alias-basic-with-alias ()
  :doc "Test `anki-editor--map-fields' should use field alias when mapping exists."
  :in "test-files/aliased-fields.org"
  :test
  (let* ((anki-editor-field-alias '(("Basic" . (("Answer" . "Back")))))
         (note (progn
                 (anki-editor-test--go-to-headline "Exercise 1.1")
                 (anki-editor-note-at-point)))
         (fields (anki-editor-note-fields note))
         (first-field (nth 0 fields))
         (second-field (nth 1 fields)))
    (should (and (string= "Back" (car first-field))
                 (string-match "2000" (cdr first-field))))
    (should (and (string= "Front" (car second-field))
                 (string-match "The last year of the 20th century was" (cdr second-field))))))

(anki-editor-deftest test--anki-editor-field-alias-cloze-with-back-extra ()
  :doc "Test `anki-editor--map-fields' should handle back extra by default."
  :in "test-files/aliased-fields.org"
  :test
  (let* ((anki-editor-swap-two-fields '("Cloze"))
         (anki-editor-field-alias nil)
         (note (progn
                 (anki-editor-test--go-to-headline "Cloze with Back Extra")
                 (anki-editor-note-at-point)))
         (fields (anki-editor-note-fields note))
         (first-field (nth 0 fields))
         (second-field (nth 1 fields)))
    (should (and (string= "Back Extra" (car first-field))
                 (string-match "This should map to Back Extra." (cdr first-field))))
    (should (and (string= "Text" (car second-field))
                 (string-match "This is the {{c1::content}}." (cdr second-field))))))

(anki-editor-deftest test--anki-editor-field-alias-cloze-with-alias ()
  :doc "Test `anki-editor--map-fields' should use field alias when mapping exists."
  :in "test-files/aliased-fields.org"
  :test
  (let* ((anki-editor-swap-two-fields '("Cloze"))
         (anki-editor-field-alias '(("Cloze" . (("Note" . "Back Extra")))))
         (note (progn
                 (anki-editor-test--go-to-headline "Cloze with Note as Back Extra")
                 (anki-editor-note-at-point)))
         (fields (anki-editor-note-fields note))
         (first-field (nth 0 fields))
         (second-field (nth 1 fields)))
    (should (and (string= "Back Extra" (car first-field))
                 (string-match "This should map to Back Extra." (cdr first-field))))
    (should (and (string= "Text" (car second-field))
                 (string-match "This is the {{c1::content}}." (cdr second-field))))))

(anki-editor-deftest test--anki-editor--build-fields-should-not-use-exclude-tags-with-nested-subheadings ()
  :doc "Test `anki-editor--build-fields' should not use exclude tags with nested subheadings."
  :in "test-files/export-exclude-tags.org"
  :test
  (let* ((org-export-exclude-tags nil)
         (note (progn
                 (anki-editor-test--go-to-headline "Basic Anki note")
                 (anki-editor-note-at-point)))
         (fields (anki-editor-note-fields note))
         (first-field (nth 0 fields))
         (second-field (nth 1 fields)))
    (should (and (string= "Back" (car first-field))
                 (string-match "This is back" (cdr first-field))
                 (string-match "This will be included in the content if" (cdr first-field))))
    (should (and (string= "Front" (car second-field))
                 (string-match "This is front" (cdr second-field))))))

;; XXX
;; (anki-editor-deftest test--anki-editor--build-fields-should-use-exclude-tags-with-nested-subheadings ()
;;   :doc "Test `anki-editor--build-fields' should use exclude tags with nested subheadings."
;;   :in "test-files/export-exclude-tags.org"
;;   :test
;;   (let* ((org-export-exclude-tags '("noexport"))
;;          (note (progn
;;                  (anki-editor-test--go-to-headline "Basic Anki note")
;;                  (anki-editor-note-at-point)))
;;          (fields (anki-editor-note-fields note))
;;          (first-field (nth 0 fields))
;;          (second-field (nth 1 fields)))
;;     (should (and (string= "Back" (car first-field))
;;                  (string-match "This is back" (cdr first-field))
;;                  (not (string-match "This will be included in the content if" (cdr first-field)))))
;;     (should (and (string= "Front" (car second-field))
;;                  (string-match "This is front" (cdr second-field))))))

(anki-editor-deftest test--anki-editor--build-fields-should-not-use-exclude-tags-in-short-form-basic ()
  :doc "Test `anki-editor--build-fields' should not use exclude tags in short form basic."
  :in "test-files/export-exclude-tags.org"
  :test
  (let* ((org-export-exclude-tags nil)
         (note (progn
                 (anki-editor-test--go-to-headline "Basic Anki note in short form")
                 (anki-editor-note-at-point)))
         (fields (anki-editor-note-fields note))
         (first-field (nth 0 fields))
         (second-field (nth 1 fields)))
    (should (and (string= "Back" (car first-field))
                 (string-match "Content" (cdr first-field))
                 (string-match "This will be included in the content if" (cdr first-field))))
    (should (and (string= "Front" (car second-field))
                 (string-match "Basic Anki note in short form" (cdr second-field))))))

;; XXX
;; (anki-editor-deftest test--anki-editor--build-fields-should-use-exclude-tags-in-short-form-basic ()
;;   :doc "Test `anki-editor--build-fields' should use exclude tags in short form basic."
;;   :in "test-files/export-exclude-tags.org"
;;   :test
;;   (let* ((org-export-exclude-tags '("noexport"))
;;          (note (progn
;;                  (anki-editor-test--go-to-headline "Basic Anki note in short form")
;;                  (anki-editor-note-at-point)))
;;          (fields (anki-editor-note-fields note))
;;          (first-field (nth 0 fields))
;;          (second-field (nth 1 fields)))
;;     (should (and (string= "Back" (car first-field))
;;                  (string-match "Content" (cdr first-field))
;;                  (not (string-match "This will be included in the content if" (cdr first-field)))))
;;     (should (and (string= "Front" (car second-field))
;;                  (string-match "Basic Anki note in short form" (cdr second-field))))))

(anki-editor-deftest test--anki-editor--build-fields-should-not-use-exclude-tags-in-short-form-cloze ()
  :doc "Test `anki-editor--build-fields' should not use exclude tags in short form Cloze."
  :in "test-files/export-exclude-tags.org"
  :test
  (let* ((org-export-exclude-tags nil)
         (note (progn
                 (anki-editor-test--go-to-headline "Cloze Anki note in short form")
                 (anki-editor-note-at-point)))
         (fields (anki-editor-note-fields note))
         (first-field (nth 0 fields))
         (second-field (nth 1 fields)))
    (should (and (string= "Back Extra" (car first-field))
                 (string-match "Content" (cdr first-field))
                 (string-match "This will be included in the content if" (cdr first-field))))
    (should (and (string= "Text" (car second-field))
                 (string-match "Cloze Anki note in short form" (cdr second-field))))))

;; XXX
;; (anki-editor-deftest test--anki-editor--build-fields-should-use-exclude-tags-in-short-form-cloze ()
;;   :doc "Test `anki-editor--build-fields' should use exclude tags in short form Cloze."
;;   :in "test-files/export-exclude-tags.org"
;;   :test
;;   (let* ((org-export-exclude-tags '("noexport"))
;;          (note (progn
;;                  (anki-editor-test--go-to-headline "Cloze Anki note in short form")
;;                  (anki-editor-note-at-point)))
;;          (fields (anki-editor-note-fields note))
;;          (first-field (nth 0 fields))
;;          (second-field (nth 1 fields)))
;;     (should (and (string= "Back Extra" (car first-field))
;;                  (string-match "Content" (cdr first-field))
;;                  (not (string-match "This will be included in the content if" (cdr first-field)))))
;;     (should (and (string= "Text" (car second-field))
;;                  (string-match "Cloze Anki note in short form" (cdr second-field))))))

(anki-editor-deftest test--anki-editor--map-fields-cloze-should-swap-heading-and-content-before-subheadings-with-property ()
  :doc "Test `anki-editor--map-fields' should swap heading and content before subheadings with property."
  :in "test-files/cloze.org"
  :test
  (let* ((anki-editor-swap-two-fields nil)
         (note (progn
                 (anki-editor-test--go-to-headline "Note with swap two fields as property")
                 (anki-editor-note-at-point)))
         (fields (anki-editor-note-fields note))
         (first-field (nth 0 fields))
         (second-field (nth 1 fields)))
    (should (and (string= "Back Extra" (car first-field))
                 (string-match "Note with swap two fields as property" (cdr first-field))))
    (should (and (string= "Text" (car second-field))
                 (string-match "This is the {{c1::content}}." (cdr second-field))))))

(anki-editor-deftest test--anki-editor--no-subheading-fields ()
  :doc "Test `anki-editor-no-subheading-fields'."
  :in "test-files/test.org"
  :test
  (let* ((note (progn
                 (anki-editor-test--go-to-headline "Quick note in a higher level heading")
                 (anki-editor-note-at-point)))
         (fields (anki-editor-note-fields note))
         (first-field (nth 0 fields))
         (second-field (nth 1 fields)))
    (should (and (string= "Back Extra" (car first-field))
                 (string-match "Lorem ipsum." (cdr first-field))))
    (should (and (string= "Text" (car second-field))
                 (string-match "This is {{c1::fast}}" (cdr second-field))))))

;;;; Push tests

(defun anki-editor-test--reset-mock ()
  "Clear the mock's recorded requests and notesInfo tag overrides."
  (anki-editor-api-call-result '__test_reset__))

(defun anki-editor-test--recorded-requests ()
  "Return the list of AnkiConnect requests the mock has recorded."
  (anki-editor-api-call-result '__test_get_requests__))

(defun anki-editor-test--set-notes-info-tags (note-id tags)
  "Make the mock's `notesInfo' return TAGS (a list of strings) for NOTE-ID."
  (anki-editor-api-call-result '__test_set_notes_info_tags__
    :noteId note-id
    :tags (vconcat tags)))

(defun anki-editor-test--find-sub-action (action requests)
  "Find the first `multi' sub-action whose action name is ACTION.
ACTION is a symbol; REQUESTS is a list of recorded request alists.
Returns the matching sub-action alist or nil."
  (cl-loop for req in requests
           when (string= "multi" (alist-get 'action req))
           thereis (cl-find-if
                    (lambda (sub)
                      (string= (alist-get 'action sub) (symbol-name action)))
                    (alist-get 'actions (alist-get 'params req)))))

(cl-defmacro anki-editor-deftest-push (name () &key doc in test)
  "Define an ERT test that loads IN into a temp org-mode buffer and runs TEST.
Like `anki-editor-deftest' but reads IN's contents into a temp buffer
rather than visiting it, so mutations made by the test (ANKI_NOTE_ID,
ANKI_NOTE_HASH, FAILURE_REASON) don't dirty the on-disk fixture."
  (declare (doc-string 3) (indent 2))
  `(ert-deftest ,name ()
     ,doc
     (save-window-excursion
       (with-temp-buffer
         (insert-file-contents
          (expand-file-name
           ,in (file-name-directory
                (symbol-file 'anki-editor-test--setup))))
         (org-mode)
         (goto-char (point-min))
         (anki-editor-test--setup)
         (cl-letf (((symbol-function 'save-buffer) #'ignore))
           (unwind-protect ,test
             (anki-editor-test--teardown)))))))

(anki-editor-deftest-push test--push-new-note-enqueues-add-note ()
  :doc "Pushing a note without an ANKI_NOTE_ID enqueues createDeck and addNote with the right deck and model."
  :in "test-files/push-new.org"
  :test
  (progn
    (anki-editor-test--reset-mock)
    (anki-editor-push-note-at-point)
    (let* ((requests (anki-editor-test--recorded-requests))
           (add-note (anki-editor-test--find-sub-action 'addNote requests))
           (create-deck (anki-editor-test--find-sub-action 'createDeck requests)))
      (should create-deck)
      (should add-note)
      (let-alist (alist-get 'note (alist-get 'params add-note))
        (should (string= "Tests" .deckName))
        (should (string= "Basic" .modelName))))))

(anki-editor-deftest-push test--push-existing-note-merges-protected-tags ()
  :doc "Protected Anki-side tags survive a push even when absent from Org."
  :in "test-files/push-existing.org"
  :test
  (let ((anki-editor-protected-tags '("marked")))
    (anki-editor-test--reset-mock)
    (anki-editor-test--set-notes-info-tags 1700000000001 '("marked" "foo"))
    (anki-editor-push-note-at-point)
    (let* ((requests (anki-editor-test--recorded-requests))
           (update-note (anki-editor-test--find-sub-action 'updateNote requests)))
      (should update-note)
      (should (anki-editor-test--find-sub-action 'changeDeck requests))
      (let-alist (alist-get 'note (alist-get 'params update-note))
        (should (member "marked" .tags))
        (should-not (member "foo" .tags))))))

(anki-editor-deftest-push test--push-existing-note-without-protected-tags ()
  :doc "Anki-side tags that aren't in `anki-editor-protected-tags' are dropped on push.
Org tags are preserved through the same path."
  :in "test-files/push-existing-tagged.org"
  :test
  (let ((anki-editor-protected-tags '("marked")))
    (anki-editor-test--reset-mock)
    (anki-editor-test--set-notes-info-tags 1700000000001 '("foo" "bar"))
    (anki-editor-push-note-at-point)
    (let* ((requests (anki-editor-test--recorded-requests))
           (update-note (anki-editor-test--find-sub-action 'updateNote requests)))
      (should update-note)
      (let-alist (alist-get 'note (alist-get 'params update-note))
        (should (member "urgent" .tags))
        (should-not (member "foo" .tags))
        (should-not (member "bar" .tags))))))

(anki-editor-deftest-push test--repush-skips-on-hash-match ()
  :doc "Re-pushing an unchanged note does not enqueue addNote or updateNote."
  :in "test-files/push-new.org"
  :test
  (let ((anki-editor-force-update nil))
    (anki-editor-test--reset-mock)
    (anki-editor-push-notes)
    (should (anki-editor-test--find-sub-action
             'addNote (anki-editor-test--recorded-requests)))
    (anki-editor-test--reset-mock)
    (anki-editor-push-notes)
    (let ((after-second (anki-editor-test--recorded-requests)))
      (should-not (anki-editor-test--find-sub-action 'addNote after-second))
      (should-not (anki-editor-test--find-sub-action 'updateNote after-second)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anki-editor-tests.el ends here
