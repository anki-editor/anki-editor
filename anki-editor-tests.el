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
     (org-attach-dir-relative . t)
     (anki-editor-prepend-heading-format . "test *%s*")
     (org-html-link-use-abs-url . nil)))
  (advice-add 'org-export-get-reference :override #'anki-editor-test--org-export-get-reference)
  (anki-editor-test-ensure-python-server)
  (anki-editor-mode 1))

(defun anki-editor-test--teardown ()
  "Teardown testing."
  (anki-editor-test--restore-variables)
  (advice-remove 'org-export-get-reference #'anki-editor-test--org-export-get-reference))

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

(defun anki-editor-test--go-to-headline (title)
  "Go to headline with TITLE."
  (goto-char (point-min))
  (re-search-forward (concat "* " title)))

(defun anki-editor-test--test-org-buffer (name)
  "Return test org buffer with NAME."
  (find-file-noselect (expand-file-name name (expand-file-name "test-files" (file-name-directory (symbol-file 'anki-editor-test--go-to-headline))))))

(ert-deftest test--note-at-point-should-return-note-at-point ()
  "Test `anki-editor--note-at-point' should return note at point."
  (save-window-excursion
    (with-current-buffer (anki-editor-test--test-org-buffer "test.org")
      (anki-editor-test--go-to-headline "Simple note")
      (anki-editor-test--setup)
      (unwind-protect
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
")) nil)))
        (anki-editor-test--teardown)))))


(ert-deftest test--note-at-point-for-note-with-property-field-should-render-property-field ()
  "Test `anki-editor--note-at-point' should render property field."
  (save-window-excursion
    (with-current-buffer (anki-editor-test--test-org-buffer "property-fields.org")
      (anki-editor-test--go-to-headline "\"Front\" property field")
      (anki-editor-test--setup)
      (unwind-protect
          (should (equal
                   (anki-editor-note-at-point)
                   #s(anki-editor-note nil "Basic" "Tests"
                                       (("Back" . "<p>\nShould be included\n</p>\n")
                                        ("Front" . "<p>\nCan one define an anki-field inside an org-mode property?</p>\n"))
                                       nil)))
        (anki-editor-test--teardown)))))

(ert-deftest test--note-at-point-for-note-with-property-field-should-override-subheading-field ()
  "Test `anki-editor--note-at-point' should override subheading field."
  (save-window-excursion
    (with-current-buffer (anki-editor-test--test-org-buffer "property-fields.org")
      (anki-editor-test--go-to-headline "\"Front\" property field with \"Front\" subheading")
      (anki-editor-test--setup)
      (unwind-protect
          (should (equal
                   (anki-editor-note-at-point)
                   #s(anki-editor-note nil "Basic" "Tests"
                                       (("Back" . "<p>\nShould be included\n</p>\n")
                                        ("Front" . "<p>\nCan one define an anki-field inside an org-mode property?</p>\n"))
                                       nil)))
        (anki-editor-test--teardown)))))


(ert-deftest test--note-at-point-for-note-with-unknown-property-field-should-raise-error ()
  "Test `anki-editor--note-at-point' should raise error for unknown property field."
  (save-window-excursion
    (with-current-buffer (anki-editor-test--test-org-buffer "property-fields.org")
      (anki-editor-test--go-to-headline "Foreign property field")
      (anki-editor-test--setup)
      (unwind-protect
          (should
           (equal
            (should-error (anki-editor-note-at-point) :type 'user-error)
            ' (user-error "Failed to map all named fields")))
        (anki-editor-test--teardown)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anki-editor-tests.el ends here
