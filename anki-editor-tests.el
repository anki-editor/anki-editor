;;; anki-editor-tests.el --- Tests for anki-editor -*- lexical-binding: t; -*-
;;
;; Filename: anki-editor-tests.el
;; Description:
;; Author: Renat Galimov
;; Maintainer:
;; Created: Thu May 11 14:05:13 2023 (+0300)
;; Version:
;; Package-Requires: ((ert))
;; Last-Updated: Mon Jun 26 08:41:59 2023 (+0300)
;;           By: Renat Galimov
;;     Update #: 256
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

(defun anki-editor-test--org-export-get-reference (datum info)
  "test")


(defvar anki-editor-test-python-server nil)

(defun anki-editor-test--start-python-server ()
  (setq anki-editor-test-python-server
        (start-process-shell-command "anki-editor-test--start-python-server" "*anki-editor-test--start-python-server*" "python3 test_server.py")))

(defun anki-editor-test--stop-python-server ()
  (kill-process "anki-editor-test--start-python-server"))


(defun anki-editor-test--patch-variables (body)

  (let ((test--anki-editor-prepend-heading-format anki-editor-prepend-heading-format)
        (test--anki-editor-org-attach-dir-relative org-attach-dir-relative)
        (test--anki-editor-org-html-link-use-abs-url org-html-link-use-abs-url)
        (test--anki-editor-api-port anki-editor-api-port))
    (unwind-protect
        (progn
          (setq anki-editor-prepend-heading-format "test *%s*"
                org-attach-dir-relative t
                anki-editor-api-port 28765)
          (anki-editor-mode 1)
          (advice-add 'org-export-get-reference :override #'anki-editor-test--org-export-get-reference)
          (funcall body))
      (advice-remove 'org-export-get-reference #'test--org-export-get-reference)
      (setq anki-editor-prepend-heading-format test--anki-editor-prepend-heading-format
            org-attach-dir-relative test--anki-editor-org-attach-dir-relative
            anki-editor-api-port test--anki-editor-api-port
            org-html-link-use-abs-url test--anki-editor-org-html-link-use-abs-url))))



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
  (find-file-noselect (expand-file-name name (expand-file-name "test-files" (file-name-directory (symbol-file 'anki-editor-test--go-to-headline))))))

(ert-deftest test--note-at-point-should-return-note-at-point ()
  "Test `anki-editor--note-at-point' should return note at point."
  (save-window-excursion
    (with-current-buffer (anki-editor-test--test-org-buffer "test.org")
      (anki-editor-test--go-to-headline "Simple note")

      (anki-editor-test--patch-variables
       (lambda ()

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
")) nil))))))))

;; (ert-deftest test--note-at-point-should-get-back-from-heading ()
;;   "Test `anki-editor--note-at-point' should get back from heading."
;;   (save-window-excursion
;;     (with-current-buffer (anki-editor-test--test-org-buffer "test.org")
;;       (anki-editor-test--go-to-headline "Note without Back")
;;       (anki-editor-test--patch-variables
;;        (lambda ()
;;          (should (equal
;;                   (anki-editor-note-at-point)
;;                   #s(anki-editor-note nil
;;                                       "Basic"
;;                                       "Tests"
;;                                       (("Back" . "<p>\ntest <b>Note without Back</b></p>\n")
;;                                        ("Front" . "<p>\nSimple note body\n</p>\n"))
;;                                       nil))))))))


(ert-deftest test--note-at-point-when-note-has-attachment-should-render-correct-link ()
  "Test `anki-editor--note-at-point' when note has attachment should render correct link."
  (save-window-excursion
    (with-current-buffer (anki-editor-test--test-org-buffer "test.org")

      (anki-editor-test--go-to-headline "Note with attachment")
      (anki-editor-test--patch-variables
       (lambda ()
         (should (equal
                  (anki-editor-note-at-point)
                  #s(anki-editor-note nil
                                      "Basic"
                                      "Tests"
                                      (("Back" . "<p>\n<a href=\"1x1-14af87ccec7f81bb28d53c84da2fd5a9d5925cda.gif\">Test image</a>\n</p>")
                                       ("Front" . "<p>\ntest <b>Note with attachment</b></p>\n")) nil))))))))


(ert-deftest test--note-at-point-when-note-has-file-link-should-render-correct-link ()
  "Test `anki-editor--note-at-point' when note has file link should render correct link."
  (save-window-excursion
    (with-current-buffer (anki-editor-test--test-org-buffer "test.org")

      (anki-editor-test--go-to-headline "Note with file link")
      (anki-editor-test--patch-variables
       (lambda ()
         (should (equal
                  (anki-editor-note-at-point)
                  #s(anki-editor-note nil
                                      "Basic"
                                      "Tests"
                                      (("Back" . "<p>\n<a href=\"1x1-14af87ccec7f81bb28d53c84da2fd5a9d5925cda.gif\">Test image</a>\n</p>\n")
                                       ("Front" . "<p>\nNote with file link\n</p>\n"))
                                      nil))))))))

(ert-deftest test--note-at-point-when-note-formatted-should-render-formatted-note()
  "Test `anki-editor--note-at-point' when note formatted should render formatted note."
  (save-window-excursion
    (with-current-buffer (anki-editor-test--test-org-buffer "test.org")

      (anki-editor-test--go-to-headline "Formatted note (formatted)")
      (anki-editor-test--patch-variables
       (lambda ()
         (should (equal
                  (anki-editor-note-at-point)
                  #s(anki-editor-note nil
                                      "Basic"
                                      "Tests"
                                      (("Back" . "<pre class=\"example\" id=\"test\">\nLorem ipsum\ndolor sit\namet\n</pre>\n")
                                       ("Front" . "<p>\n<i>Simple</i> <b>note</b> <code>body</code>\n</p>\n"))
                                      nil))))))))

(ert-deftest test--note-at-point-when-note-formatted-but-formatting-disabled-should-render-unformatted-note()
  "Test `anki-editor--note-at-point' when note formatted should render formatted note."
  (save-window-excursion
    (with-current-buffer (anki-editor-test--test-org-buffer "test.org")

      (anki-editor-test--go-to-headline "Formatted note (plain)")
      (anki-editor-test--patch-variables
       (lambda ()
         (should (equal
                  (anki-editor-note-at-point)
                  #s(anki-editor-note nil
                                      "Basic"
                                      "Tests"
                                      (("Back" . "#+begin_example\nLorem ipsum\ndolor sit\namet\n#+end_example\n")
                                       ("Front" . "/Simple/ *note* =body=\n"))
                                      nil))))))))


;; The problem with this behavior is that it gets "Back Extra" from
;; the headline. I'd prefer back-extra to be empty.
(ert-deftest test--note-at-point-for-cloze-with-front-should-render-note()
  "Test `anki-editor--note-at-point' for cloze with front should render note."
  (save-window-excursion
    (with-current-buffer (anki-editor-test--test-org-buffer "test.org")

        (anki-editor-test--go-to-headline "Cloze with text")
        (anki-editor-test--patch-variables
         (lambda ()
             (should (equal
                    (anki-editor-note-at-point)
                    #s(anki-editor-note nil
                                        "Cloze"
                                        "Tests"
                                        (("Back Extra" . "<p>\ntest <b>Cloze with text</b></p>\n")
                                         ("Text" . "<p>\nLorem {{c1::lorem}}.\n</p>\n"))
                                        nil))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anki-editor-tests.el ends here
