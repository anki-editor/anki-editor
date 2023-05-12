;;; anki-editor-tests.el --- Tests for anki-editor -*- lexical-binding: t; -*-
;;
;; Filename: anki-editor-tests.el
;; Description:
;; Author: Renat Galimov
;; Maintainer:
;; Created: Thu May 11 14:05:13 2023 (+0300)
;; Version:
;; Package-Requires: ((ert))
;; Last-Updated: Fri May 12 22:07:12 2023 (+0300)
;;           By: Renat Galimov
;;     Update #: 40
;; URL: https://github.com/orgtre/anki-editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'ert)
(require 'anki-editor)


(defun anki-editor-test--patch-variables (body)
  (let ((anki-editor-prepend-heading-format "/%s/"))
    (funcall body)))

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

  (should (equal (anki-editor--concat-fields '("Front" "Back")
                                             '(("Front" . "Front content"))
                                             0)
                 "* Front

Front content

")))


(defun anki-editor-test--go-to-headline (title)
  "Go to headline with TITLE."
  (goto-char (point-min))
  (re-search-forward (concat "* " title)))

(defun anki-editor-test--test-org-buffer (name)
  "Return test org buffer with NAME."
  (find-file-noselect (expand-file-name name (expand-file-name "test-files" (file-name-directory (symbol-file 'anki-editor--test-go-to-headline))))))

(ert-deftest test--note-at-point-should-return-note-at-point ()
  "Test `anki-editor--note-at-point' should return note at point."
  (save-window-excursion
    (with-current-buffer (anki-editor-test--test-org-buffer "test.org")
      (anki-editor-test--go-to-headline "Simple note")
      (should (equal
               (anki-editor-note-at-point)
               #s(anki-editor-note nil
                                   "Basic"
                                   "Tests"
                                   (("Back" . "<p>
Lorem
</p>
")
                                    ("Front" . "<p>
Simple note body
</p>
")) nil))))))

(ert-deftest test--note-at-point-should-get-back-from-heading ()
  "Test `anki-editor--note-at-point' should get back from heading."
  (save-window-excursion
    (with-current-buffer (anki-editor-test--test-org-buffer "test.org")

      (anki-editor-test--go-to-headline "Note without Back")
      (anki-editor-test--patch-variables
       (lambda ()
         (should (equal
                  (anki-editor-note-at-point)
                  #s(anki-editor-note nil
                                      "Basic"
                                      "Tests"
                                      (("Back" . "<p>
<i>Note without Back</i></p>
")
                                       ("Front" . "<p>
Simple note body
</p>
")) nil))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anki-editor-tests.el ends here
