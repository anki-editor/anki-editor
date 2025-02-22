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
    (should (equal (anki-editor--export-string "# raw content" t) "content"))
    (should (equal (anki-editor--export-string "# raw  content" t) "content"))
    (should (equal (anki-editor--export-string "# raw\ncontent" t) "content"))
    (should (equal (anki-editor--export-string "# raw" t) ""))))

(anki-editor-deftest test--export-string-without-raw ()
  :doc "Test `anki-editor--export-string` without `# raw` prefix."
  :in "test-files/test.org"
  :test
  (progn
    (should (equal (anki-editor--export-string "content" t) "<p>\ncontent</p>\n"))
    (should (equal (anki-editor--export-string "" t) ""))))

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
  (progn (anki-editor-test--go-to-headline "Simple note")
         (should (equal
                  (anki-editor-note-at-point)
                  #s(anki-editor-note nil "Basic" "Tests"
                                      (("Back" . "<p>
Lorem
</p>
")
                                       ("Front" . "<p>
Simple note body
</p>
")) nil)))))

(anki-editor-deftest test--note-at-point-for-note-with-property-field-should-render-property-field ()
  :doc "Test `anki-editor--note-at-point' should render property field."
  :in "test-files/property-fields.org"
  :test
  (progn (anki-editor-test--go-to-headline "\"Front\" property field")
         (should (equal
                  (anki-editor-note-at-point)
                  #s(anki-editor-note nil "Basic" "Tests"
                                      (("Back" . "<p>\nShould be included\n</p>\n")
                                       ("Front" . "<p>\nCan one define an anki-field inside an org-mode property?</p>\n"))
                                      nil)))))

(anki-editor-deftest test--note-at-point-for-note-with-property-field-should-override-subheading-field ()
  :doc "Test `anki-editor--note-at-point' should override subheading field."
  :in "test-files/property-fields.org"
  :test
  (progn (anki-editor-test--go-to-headline "\"Front\" property field with \"Front\" subheading")
         (should (equal
                  (anki-editor-note-at-point)
                  #s(anki-editor-note nil "Basic" "Tests"
                                      (("Back" . "<p>\nShould be included\n</p>\n")
                                       ("Front" . "<p>\nCan one define an anki-field inside an org-mode property?</p>\n"))
                                      nil)))))

(anki-editor-deftest test--note-at-point-for-examples-should-produce-correct-output ()
  :doc "Test `anki-editor--note-at-point' should produce correct output for examples."
  :in "examples.org"
  :test
  (let ((test-items-alist
         '(("Deck in file" . #s(anki-editor-note
                                nil "Cloze" "Default"
                                (("Back Extra" . "<p>\nDeck in file</p>\n")
                                 ("Text" . "<p>\nCards of this note will be created in {{c1::Default::which deck?}}\n</p>\n"))
                                nil))
           ("Deck in entry" . #s(anki-editor-note
                                 nil "Cloze" "Languages"
                                 (("Back Extra" . "<p>\nDeck in entry</p>\n")
                                  ("Text" . "<p>\nCards of this note will be created in {{c1::Languages::which deck?}}\n</p>\n"))
                                 nil))
           ("Raw fields" . #s(anki-editor-note
                              nil "Basic" "Default"
                              (("Back" . "<p>\nWith property &lt;code&gt;:ANKI<sub>FORMAT</sub>: nil&lt;/code&gt;, content of the\nfield will be sent to Anki &lt;em&gt;unprocessed&lt;/em&gt;.  You can use\nwhatever Anki supports, like HTML tags.\n&lt;br&gt;\n&lt;br&gt;\nThis property is retrieved with inheritance, meaning that it can be\nset in any ancestor entries or at the top of the file with\n&lt;code&gt;#+PROPERTY: ANKI<sub>FORMAT</sub> nil&lt;/code&gt;, it's also possible to\noverride an outer level nil format with &lt;code&gt;:ANKI<sub>FORMAT</sub>: t&lt;/code&gt;.\n</p>\n")
                               ("Front" . "<p>\nHow to send the content of a field or fields to Anki as is?\n</p>\n"))
                              nil))
           ("Is there a shorter way to write notes?" . #s(anki-editor-note
                                                          nil "Basic" "Default"
                                                          (("Back" . "<p>\nYes, like this one, Front is missing, <code>anki-editor</code> will use note\nheading as Front.  This is neat as sometimes it's verbose to repeat\nthe same content in note heading and first field.\n</p>\n\n<p>\nThis works for all note types, just make one field absent and\n<code>anki-editor</code> will use note heading as that missing field.\n</p>\n")
                                                           ("Front" . "<p>\nIs there a shorter way to write notes?</p>\n"))
                                                          nil))
           ("Raining" . #s(anki-editor-note
                           nil "Basic (and reversed card)" "Languages"
                           (("Back" . "<p>\nit's raining very hard\n</p>\n")
                            ("Front" . "<p>\n(it's) raining cats and dogs\n</p>\n"))
                           ("vocab" "idioms" "english")))
           ("名词从句" . #s(anki-editor-note
                            nil "Basic" "Languages"
                            (("Back" . "<ol class=\"org-ol\">\n<li>That + 一个完整的句子, that无实际意义</li>\n<li>由疑问句改装而成</li>\n</ol>\n")
                             ("Front" . "<p>\n名词从句有哪些形式？\n</p>\n"))
                            ("grammar" "english")))
           ("Cantonese" . #s(anki-editor-note
                             nil "Basic (and reversed card)" "Languages"
                             (("Back" . "<p>\n吃过饭了没？\n</p>\n")
                              ("Front" . "<p>\n食咗饭未吖？\n</p>\n"))
                             ("cantonese" "dialect")))
           ("Emacs Lisp" . #s(anki-editor-note
                              nil "Basic" "Computing"
                              (("Back" . "<div align=\"left\">\n\n<div class=\"org-src-container\">\n<pre class=\"src src-emacs-lisp\">(condition-case the-error\n    ;; the protected form\n    (progn\n      (do-something-dangerous)\n      (do-something-more-dangerous))\n  ;; handlers\n  (error-symbol1 (handler1 the-error))\n  ((error-symbol2 error-symbol3 (handler the-error))))\n</pre>\n</div>\n\n</div>\n")
                               ("Front" . "<p>\nHow to trap errors in emacs lisp?\n</p>\n"))
                              ("lisp" "emacs" "programming")))
           ("Dot product" . #s(anki-editor-note
                               nil "Basic" "Mathematics"
                               (("Back" . "<p>\n<p>[$$]\\alpha \\cdot \\beta = a_1b_1 + a_2b_2 + a_3b_3[/$$]</p>\n</p>\n")
                                ("Front" . "<p>\nHow to calculate the dot product of two vectors:\n</p>\n\n[latex]<br>\\begin{equation*}<br>\\alpha = \\{a_1, a_2, a_3\\}, \\beta = \\{b_1, b_2, b_3\\}<br>\\end{equation*}<br>[/latex]\n"))
                               nil))
           )))
    (org-map-entries
     (lambda ()
       ;; Only entries which have ANKI_NOTE_TYPE
       (when (org-entry-get (point) "ANKI_NOTE_TYPE")
         (unwind-protect
             (let ((note-at-point nil)
                   (headline nil)
                   (expected-note nil))
               (setq headline (org-entry-get (point) "ITEM"))
               (setq note-at-point (anki-editor-note-at-point))
               (setq expected-note (cdr (assoc headline test-items-alist)))
               (should (equal note-at-point expected-note)))))))))

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

(anki-editor-deftest test--anki-editor--build-fields-should-use-exclude-tags-with-nested-subheadings ()
  :doc "Test `anki-editor--build-fields' should use exclude tags with nested subheadings."
  :in "test-files/export-exclude-tags.org"
  :test
  (let* ((org-export-exclude-tags '("noexport"))
         (note (progn
                 (anki-editor-test--go-to-headline "Basic Anki note")
                 (anki-editor-note-at-point)))
         (fields (anki-editor-note-fields note))
         (first-field (nth 0 fields))
         (second-field (nth 1 fields)))
    (should (and (string= "Back" (car first-field))
                 (string-match "This is back" (cdr first-field))
                 (not (string-match "This will be included in the content if" (cdr first-field)))))
    (should (and (string= "Front" (car second-field))
                 (string-match "This is front" (cdr second-field))))))

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

(anki-editor-deftest test--anki-editor--build-fields-should-use-exclude-tags-in-short-form-basic ()
  :doc "Test `anki-editor--build-fields' should use exclude tags in short form basic."
  :in "test-files/export-exclude-tags.org"
  :test
  (let* ((org-export-exclude-tags '("noexport"))
         (note (progn
                 (anki-editor-test--go-to-headline "Basic Anki note in short form")
                 (anki-editor-note-at-point)))
         (fields (anki-editor-note-fields note))
         (first-field (nth 0 fields))
         (second-field (nth 1 fields)))
    (should (and (string= "Back" (car first-field))
                 (string-match "Content" (cdr first-field))
                 (not (string-match "This will be included in the content if" (cdr first-field)))))
    (should (and (string= "Front" (car second-field))
                 (string-match "Basic Anki note in short form" (cdr second-field))))))

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

(anki-editor-deftest test--anki-editor--build-fields-should-use-exclude-tags-in-short-form-cloze ()
  :doc "Test `anki-editor--build-fields' should use exclude tags in short form Cloze."
  :in "test-files/export-exclude-tags.org"
  :test
  (let* ((org-export-exclude-tags '("noexport"))
         (note (progn
                 (anki-editor-test--go-to-headline "Cloze Anki note in short form")
                 (anki-editor-note-at-point)))
         (fields (anki-editor-note-fields note))
         (first-field (nth 0 fields))
         (second-field (nth 1 fields)))
    (should (and (string= "Back Extra" (car first-field))
                 (string-match "Content" (cdr first-field))
                 (not (string-match "This will be included in the content if" (cdr first-field)))))
    (should (and (string= "Text" (car second-field))
                 (string-match "Cloze Anki note in short form" (cdr second-field))))))

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

(ert-deftest test--note-at-point-for-test-notes-org--should-produce-correct-output()
  (let ((test-items-alist
         '(("Basic note with subheadings" .
            #s(anki-editor-note nil "Basic" "Test"
                                (("Back" . "\
<p>
<b>Back</b>
</p>
<div id=\"outline-container-test\" class=\"outline-2\">
<h2 id=\"test\"><span class=\"section-number-2\">1.</span> Subheading</h2>
<div class=\"outline-text-2\" id=\"text-1\">
<p>
Content of subheading
</p>
</div>


<div id=\"outline-container-test\" class=\"outline-3\">
<h3 id=\"test\"><span class=\"section-number-3\">1.1.</span> Subsubheading</h3>
<div class=\"outline-text-3\" id=\"text-1-1\">
<p>
Content of subsubheading
</p>
</div>
</div>
</div>
")
                                 ("Front" . "\
<p>
<b>Front</b>
<i>Basic note with subheadings</i>
<code>Works</code>
</p>
")) nil))

           ("Basic note with Back subheading but no text before" .
            #s(anki-editor-note nil "Basic" "Test"
                                (("Back" . "\
<div id=\"outline-container-test\" class=\"outline-2\">
<h2 id=\"test\"><span class=\"section-number-2\">1.</span> Subheading</h2>
<div class=\"outline-text-2\" id=\"text-1\">
<p>
Content of subheading
</p>
</div>
</div>
")
                                 ("Front" . "<p>
<b>Front</b>
<i>Basic note with Back subheadings but no text before</i>
<code>Works</code>
</p>
")) nil))

           ("*Front* of /Basic note with omitted Front and Back/" .
            #s(anki-editor-note nil "Basic" "Test"
                                (("Back" . "\
<p>
<b>Back</b>
<code>Works</code>
</p>
<ul class=\"org-ul\">
<li>The heading is mapped to the first missing field: Front.</li>
<li>The content is mapped to the second missing field: Back.</li>
</ul>\n")
                                 ("Front" . "\
<p>
<b>Front</b> of <i>Basic note with omitted Front and Back</i></p>
")) nil))

           ("*Front* of /Basic note with omitted Front and Back, and subheading/" .
            #s(anki-editor-note nil "Basic" "Test"
                                (("Back" . "\
<p>
<b>Back</b>
<code>Fails</code>
</p>
<ul class=\"org-ul\">
<li>The subheading is omitted.</li>
</ul>


<div id=\"outline-container-test\" class=\"outline-2\">
<h2 id=\"test\"><span class=\"section-number-2\">1.</span> Subheading</h2>
<div class=\"outline-text-2\" id=\"text-1\">
<p>
Content of subheading
</p>
</div>
</div>
")
                                 ("Front" . "\
<p>
<b>Front</b> of <i>Basic note with omitted Front and Back, and subheading</i></p>
")) nil))
           ("Basic note with omitted Front, Back field heading, and text before" .
            #s(anki-editor-note nil "Basic" "Test"
                                (("Back" . "\
<p>
<b>Back</b>
</p>
")
                                 ("Front" . "\
<p>
<b>Front</b>
<i>Basic note with omitted Front, Back field heading, and text before</i>
<code>Works</code>
</p>
<ul class=\"org-ul\">
<li>The text before first heading is mapped to the front field.</li>
</ul>
")) nil))

           ("*Front* of /Basic note with omitted Front, Back field heading, and no text before/" .
            #s(anki-editor-note nil "Basic" "Test"
                                (("Back" . "\
<p>
<b>Back</b>
<code>Works</code>
</p>
")
                                 ("Front" . "\
<p>
<b>Front</b> of <i>Basic note with omitted Front, Back field heading, and no text before</i></p>
")) nil))

           ("Note type with three fields: field1-field2-field3" .
            #s(anki-editor-note nil "field1-field2-field3" "Test"
                                (("field1" . "\
<p>
<b>field1</b>
<i>Note type with three fields: field1-field2-field3</i>
<code>Works</code>
</p>
")
                                 ("field2" . "\
<p>
<b>field2</b>
</p>
")
                                 ("field3" . "\
<p>
<b>field3</b>
</p>
")) nil))
           ("field1-field2-field3 note, field2 omitted, text before first heading" .
            #s(anki-editor-note nil "field1-field2-field3" "Test"
                                (("field1" . "\
<p>
<b>field1</b>
<i>field1-field2-field3 note, field2 omitted, text before first heading</i>
<code>Works</code>
</p>
<ul class=\"org-ul\">
<li>The text before the first heading is mapped to the missing field.</li>
</ul>
")
                                 ("field2" . "\
<p>
<b>field2</b>
</p>
")
                                 ("field3" . "\
<p>
<b>field3</b>
</p>
")) nil))

           ("field1-field2-field3 note, field2 omitted, text before first heading, subheading in field1" .
            #s(anki-editor-note nil "field1-field2-field3" "Test"
                                (("field1" . "\
<p>
<b>field1</b>
<i>field1-field2-field3 note, field2 omitted, text before first heading, subheading in field1</i>
<code>Works</code>
</p>


<div id=\"outline-container-test\" class=\"outline-2\">
<h2 id=\"test\"><span class=\"section-number-2\">1.</span> Subheading</h2>
<div class=\"outline-text-2\" id=\"text-1\">
<p>
Content of subheading
</p>
</div>

<div id=\"outline-container-test\" class=\"outline-3\">
<h3 id=\"test\"><span class=\"section-number-3\">1.1.</span> Subsub</h3>
</div>
</div>
")
                                 ("field2" . "\
<p>
<b>field2</b>
</p>
")
                                 ("field3" . "\
<p>
<b>field3</b>
</p>
")) nil))

           ("*field2* of /field1-field2-field3, field2 omitted, no text before first heading/" .
            #s(anki-editor-note nil "field1-field2-field3" "Test"
                                (("field1" . "\
<p>
<b>field1</b>
<code>Works</code>
</p>
")
                                 ("field2" . "\
<p>
<b>field2</b> of <i>field1-field2-field3, field2 omitted, no text before first heading</i></p>
")
                                 ("field3" . "\
<p>
<b>field3</b>
</p>
")) nil))
           ("*field1* of /field1-field2-field3 note, two omitted, text before first heading/" .
            #s(anki-editor-note nil "field1-field2-field3" "Test"
                                (("field1" . "\
<p>
<b>field1</b> of <i>field1-field2-field3 note, two omitted, text before first heading</i></p>
")
                                 ("field2" . "\
<p>
<b>field2</b>
<code>Works</code>
</p>
<ul class=\"org-ul\">
<li>The heading is mapped to the first missing field.</li>
<li>Text before first heading is mapped to second missing field.</li>
</ul>
")
                                 ("field3" . "\
<p>
<b>field3</b>
</p>
")) nil))
           ("*Front* of /Basic note with Mathjax line break/" .
            #s(anki-editor-note nil "Basic" "Test"
                                (("Back" . "\
<p>
<b>Back</b>
<code>Works</code>
</p>

[latex]<br>\\begin{align*}<br>\\forall \\epsilon &gt; 0, \\exists \\delta &gt; 0, \\forall y, \\\\<br>\\lvert x - y \\rvert &lt; \\delta \\implies \\lvert f(x) - f(y) \\rvert &lt; \\epsilon.<br>\\end{align*}<br>[/latex]
")
                                 ("Front" . "\
<p>
<b>Front</b> of <i>Basic note with Mathjax line break</i></p>
")) nil))
           ("*Front* of /Basic (and reversed card)/" .
            #s(anki-editor-note nil "Basic (and reversed card)" "Test"
                                (("Back" . "\
<p>
<b>Back</b>
<code>Works</code>
</p>
")
                                 ("Front" . "\
<p>
<b>Front</b> of <i>Basic (and reversed card)</i></p>
")) nil))
           ("Basic note with empty Back" .
            #s(anki-editor-note nil "Basic" "Test"
                                (("Back" . "")
                                 ("Front" . "\
<p>
<b>Front</b>
<i>Basic note with empty Back</i>
<code>Works</code>
</p>
")) nil))
           ("*Front* of /Empty Basic note/" .
            #s(anki-editor-note nil "Basic" "Test"
                                (("Back" . "")
                                 ("Front" . "\
<p>
<b>Front</b> of <i>Empty Basic note</i></p>
")) nil))

           ("BASIC note with heading prepended and extra heading" .
            #s(anki-editor-note nil "Basic" "Test"
                                (("Back" . "\
<p>
Answer here.
</p>
")
                                 ("Front" . "\
<p>
test *BASIC note with heading prepended and extra heading*Some text here is required.
</p>

<div id=\"outline-container-test\" class=\"outline-2\">
<h2 id=\"test\"><span class=\"section-number-2\">1.</span> Extra heading</h2>
<div class=\"outline-text-2\" id=\"text-1\">
<p>
Text of extra heading.
</p>
</div>
</div>
")) nil))
           ("Cloze with headings" .
            #s(anki-editor-note nil "Cloze" "Test"
                                (("Back Extra" . "")
                                 ("Text" . "<p>
The capital of Norway is {{c1::Oslo.}}
</p>
")) nil))
           ("Basic note with heading prepended" .
            #s(anki-editor-note nil "Basic" "Test"
                                (("Back" . "\
<p>
Back
</p>
")
                                 ("Front" . "\
<p>
test *Basic note with heading prepended*Content
</p>
")) nil))

           ("Tags property test" .
            #s(anki-editor-note nil "Basic" "Test"
                                (("Back" . "\
<p>
note with tag ccc
</p>
")
                                 ("Front" . "\
<p>
Tags property test</p>
"))
                                ("ccc")))
           ("Field from property" .
            #s(anki-editor-note nil "Basic" "Test"
                                (("Back" . "\
<p>
Back field
</p>

<p>
<code>Doesn't work: With anki-editor-prepend-heading non-nil, the heading is prepended since property fields are not counted when determining whether to do so or not.</code>
</p>
")
                                 ("Front" . "\
<p>
Front field from property</p>
")) nil))
           ("Two fields from property" .
            #s(anki-editor-note nil "field1-field2-field3" "Test"
                                (("field1" . "\
<p>
Field1 from property</p>
")
                                 ("field2" . "\
<p>
Field2 from contents
<code>Works</code>
</p>
")
                                 ("field3" . "\
<p>
Field3 from property</p>
")) nil))

           ("Note with image link" .
            #s(anki-editor-note nil "Basic" "Test"
                                (("Back" . "\
<p>
Image link: <img src=\"1x1.jpg\"></a>
</p>
")
                                 ("Front" . "\
<p>
Note with image link</p>
")) nil))

           ("Last note with empty Back" .
            #s(anki-editor-note nil "Basic" "Test"
                                (("Back" . "")
                                 ("Front" . "\
<p>
<b>Front</b>
<i>Last note with empty Back</i>
<code>Works</code>
</p>
")) nil)))))
    (save-window-excursion
      (with-current-buffer (anki-editor-test--test-org-buffer "test-files/test-notes.org")
        (anki-editor-test--setup)
        (unwind-protect
            (org-map-entries
             (lambda ()
               ;; Only entries which have ANKI_NOTE_TYPE
               (when (org-entry-get (point) "ANKI_NOTE_TYPE")
                 (unwind-protect
                     (let ((note-at-point nil)
                           (headline nil)
                           (expected-note nil))
                       (setq headline (org-entry-get (point) "ITEM"))
                       (setq note-at-point (anki-editor-note-at-point))
                       (setq expected-note (cdr (assoc headline test-items-alist)))
                       (unless expected-note
                         (error "No expected note found for headline: '%s'" headline))
                       (message "Testing %s headline" headline)
                       (should (equal note-at-point expected-note)))))))
          (anki-editor-test--teardown))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anki-editor-tests.el ends here
