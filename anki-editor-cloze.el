;;; anki-editor-cloze.el --- Minor mode for Cloze deletion  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022 Lei Tan <louietanlei[at]gmail[dot]com>
;;               2022–2026 anki-editor contributors

;; Author: Lei Tan
;; Version: 0.3.4
;; URL: https://github.com/anki-editor/anki-editor
;; Package-Requires: ((emacs "29.1"))

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

;;; Commentary:
;;
;; `anki-editor-cloze-mode' is a minor mode for visual concealment and highlighting of
;; Anki cloze deletions in Org documents.
;;
;; When activated, the appearance of cloze deletions are customized
;; through font lock based on display text properties and face
;; attributes.

;;; Code:

(require 'font-lock)
(require 'subr-x)

(defgroup anki-editor-cloze nil
  "Visual concealment and highlighting for Anki cloze deletions."
  :prefix "anki-editor-cloze-"
  :group 'anki-editor)

(defcustom anki-editor-cloze-label-display '(raise 0.45)
  "Display property specification for single-line cloze deletion labels."
  :type '(choice (const :tag "None" nil)
                 (sexp :tag "Custom display property"))
  :group 'anki-editor-cloze)

(defcustom anki-editor-cloze-multiline-label-display nil
  "Display property specification for multiline cloze deletion labels."
  :type '(choice (const :tag "None" nil)
                 (sexp :tag "Custom display property"))
  :group 'anki-editor-cloze)

(defcustom anki-editor-cloze-payload-opening-display nil
  "Display property specification for single-line cloze payload opening."
  :type '(choice (const :tag "None" nil)
                 (sexp :tag "Custom display property"))
  :group 'anki-editor-cloze)

(defcustom anki-editor-cloze-payload-closing-display nil
  "Display property specification for single-line cloze payload closing."
  :type '(choice (const :tag "None" nil)
                 (sexp :tag "Custom display property"))
  :group 'anki-editor-cloze)

(defcustom anki-editor-cloze-multiline-payload-opening-display "{"
  "Opening delimiter string or display spec for multiline cloze payloads."
  :type '(choice (const :tag "None" nil)
                 (sexp :tag "Custom display property"))
  :group 'anki-editor-cloze)

(defcustom anki-editor-cloze-multiline-payload-closing-display "}"
  "Closing delimiter string or display spec for multiline cloze payloads."
  :type '(choice (const :tag "None" nil)
                 (sexp :tag "Custom display property"))
  :group 'anki-editor-cloze)

(defcustom anki-editor-cloze-search-limit 3000
  "Maximum character bound for searching for cloze boundaries."
  :type 'natnum
  :group 'anki-editor-cloze)

(defcustom anki-editor-cloze-label-display-method 'inline
  "Method used to render cloze deletion labels (e.g. `c1')."
  :type '(choice (const :tag "Hide labels completely" nil)
                 (const :tag "Show labels inline" inline)
                 (const :tag "Show labels in help-echo tooltip/echo area" help-echo))
  :group 'anki-editor-cloze)

;;; Faces

(defface anki-editor-cloze-paren-face
  '((t :inherit font-lock-comment-delimiter-face))
  "Face used for cloze deletion delimiter brackets."
  :group 'anki-editor-cloze)

(defface anki-editor-cloze-label
  '((t :foreground "deep sky blue" :height 0.75))
  "Face used for rendering inline cloze labels."
  :group 'anki-editor-cloze)

(defface anki-editor-cloze-payload-face
  '((((background dark)) :foreground "#696969" )
    (t :foreground "#696969" :underline (:style dashes)))
  "Base face used for cloze deletion payload text."
  :group 'anki-editor-cloze)

(defface anki-editor-cloze-payload-face-1
  '((t :inherit anki-editor-cloze-payload-face :underline (:style dashes :color "deep sky blue")))
  "Face used for cloze deletion payloads with label index 1."
  :group 'anki-editor-cloze)

(defface anki-editor-cloze-payload-face-2
  '((t :inherit anki-editor-cloze-payload-face :underline (:style dashes :color "forest green")))
  "Face used for cloze deletion payloads with label index 2."
  :group 'anki-editor-cloze)

(defface anki-editor-cloze-payload-face-3
  '((t :inherit anki-editor-cloze-payload-face :underline (:style dashes :color "dark violet")))
  "Face used for cloze deletion payloads with label index 3."
  :group 'anki-editor-cloze)

(defface anki-editor-cloze-payload-face-4
  '((t :inherit anki-editor-cloze-payload-face :underline (:style dashes :color "salmon")))
  "Face used for cloze deletion payloads with label index 4."
  :group 'anki-editor-cloze)

(defface anki-editor-cloze-multiline-payload-face
  '((((background dark)) :background "#2a2a2a")
    (t :background "#efefef"))
  "Face used for multiline cloze deletion payload regions."
  :group 'anki-editor-cloze)

(defface anki-editor-cloze-payload-cursor-face
  '((t nil))
  "Base face applied to cloze payload text when point is inside it."
  :group 'anki-editor-cloze)

(defface anki-editor-cloze-payload-cursor-face-1
  '((t :inherit anki-editor-cloze-payload-cursor-face :foreground "deep sky blue"
       :underline (:style line :color "deep sky blue")))
  "Cursor highlight face for cloze deletion label index 1."
  :group 'anki-editor-cloze)

(defface anki-editor-cloze-payload-cursor-face-2
  '((t :inherit anki-editor-cloze-payload-cursor-face :foreground "forest green"
       :underline (:style line :color "forest green")))
  "Cursor highlight face for cloze deletion label index 2."
  :group 'anki-editor-cloze)

(defface anki-editor-cloze-payload-cursor-face-3
  '((t :inherit anki-editor-cloze-payload-cursor-face :foreground "dark violet"
       :underline (:style line :color "dark violet")))
  "Cursor highlight face for cloze deletion label index 3."
  :group 'anki-editor-cloze)

(defface anki-editor-cloze-payload-cursor-face-4
  '((t :inherit anki-editor-cloze-payload-cursor-face :foreground "salmon"
       :underline (:style line :color "salmon")))
  "Cursor highlight face for cloze deletion label index 4."
  :group 'anki-editor-cloze)

(defface anki-editor-cloze-multiline-payload-cursor-face
  '((t :inherit anki-editor-cloze-payload-cursor-face :underline (:style dashes :color "deep sky blue")))
  "Base face applied to multiline cloze payload text when point is inside it."
  :group 'anki-editor-cloze)

(defconst anki-editor-cloze-payload-faces
  [anki-editor-cloze-payload-face-1
   anki-editor-cloze-payload-face-2
   anki-editor-cloze-payload-face-3
   anki-editor-cloze-payload-face-4]
  "Vector of faces used cyclically for cloze payload highlighting.")

(defconst anki-editor-cloze-payload-cursor-faces
  [anki-editor-cloze-payload-cursor-face-1
   anki-editor-cloze-payload-cursor-face-2
   anki-editor-cloze-payload-cursor-face-3
   anki-editor-cloze-payload-cursor-face-4]
  "Vector of faces used cyclically when cursor is over a cloze payload.")

(defconst anki-editor-cloze-opening-regexp "\\({{\\)\\(c[0-9]+\\)\\(::\\)"
  "Regexp matching the opening delimiter of a cloze deletion tag.")

(defconst anki-editor-cloze-closing-regexp "}}"
  "Regexp matching the closing delimiter of a cloze deletion tag.")

(defconst anki-editor-cloze-newline-regexp "\n"
  "Regexp matching a newline character.")

(defconst anki-editor-cloze-tag-regexp
  (concat "\\(?:" anki-editor-cloze-closing-regexp
          "\\|" anki-editor-cloze-opening-regexp "\\)")
  "Combined regexp matching any cloze structural token.")

(defconst anki-editor-cloze-tag-multiline-regexp
  (concat "\\(?:" anki-editor-cloze-tag-regexp
          "\\|" anki-editor-cloze-newline-regexp "\\)")
  "Combined regexp matching any cloze structural token or newline.")

(defvar-local anki-editor-nested--last nil
  "Cons cell (BEG . END) storing bounds of the last fontified cloze block.")

;;; Internal Functions

(defun anki-editor-cloze--pick-face (faces label-num)
  "Select a face symbol from vector FACES corresponding to 1-indexed LABEL-NUM."
  (aref faces (mod (1- label-num) (length faces))))

(defun anki-editor-cloze-start-position ()
  "Return the buffer start position of the cloze block at point.
Returns nil if point is outside any cloze deletion."
  (save-excursion
    (let ((limit (max (point-min) (- (point) anki-editor-cloze-search-limit)))
          (closed-tags-count 0)
          (start-pos nil))
      (while (and (not start-pos)
                  (re-search-backward anki-editor-cloze-tag-regexp limit t))
        (if (string-match-p anki-editor-cloze-closing-regexp (match-string 0))
            (setq closed-tags-count (1+ closed-tags-count))
          (if (> closed-tags-count 0)
              (setq closed-tags-count (1- closed-tags-count))
            (setq start-pos (match-beginning 0)))))
      start-pos)))

(defun anki-editor-cloze-inside-p ()
  "Return non-nil if point is currently inside a cloze deletion block."
  (not (null (anki-editor-cloze-start-position))))

(defun anki-editor-cloze-end-position ()
  "Return the buffer end position of the current cloze block.
Returns nil if the cloze block is unbalanced or unclosed before
`anki-editor-cloze-search-limit'."
  (when-let* ((start (anki-editor-cloze-start-position)))
    (save-excursion
      (goto-char start)
      (when (looking-at anki-editor-cloze-opening-regexp)
        (goto-char (match-end 0)))
      (let ((depth 1)
            (limit (min (point-max) (+ (point) anki-editor-cloze-search-limit))))
        (while (and (> depth 0)
                    (re-search-forward anki-editor-cloze-tag-regexp limit t))
          (if (string-match-p anki-editor-cloze-closing-regexp (match-string 0))
              (setq depth (1- depth)) ; stepped out of a layer
            (setq depth (1+ depth)))) ; stepped into a nested layer
        (when (= depth 0)
          (point))))))

(defun anki-editor-cloze-extend-region ()
  "Extend font-lock region boundaries to avoid truncating cloze blocks.
Intended for use in `font-lock-extend-region-functions'."
  (let ((changed nil))
    ;; If `font-lock-beg' is inside a multiline block, move it back:
    (save-excursion
      (goto-char font-lock-beg)
      (when (and (not (bobp)) (anki-editor-cloze-inside-p))
        (setq font-lock-beg (anki-editor-cloze-start-position)
              changed t)))
    ;; If `font-lock-end' cuts off a block, push it forward:
    (save-excursion
      (goto-char font-lock-end)
      (when (and (not (eobp)) (anki-editor-cloze-inside-p))
        (when-let* ((end (anki-editor-cloze-end-position)))
          (setq font-lock-end end
                changed t))))
    changed))

(defun anki-editor-cloze-find-balanced-end (limit)
  "Scan forward from point to find the matching '}}' delimiter up to LIMIT.
Accounts for nested cloze blocks.  Returns a cons cell (END-POS . MULTILINE-P)
or nil if unbalanced."
  (let ((depth 1) multiline)
    (catch :exit
      (while-let ((matched
                   (and (> depth 0)
                        (re-search-forward anki-editor-cloze-tag-multiline-regexp limit t)
                        (match-string 0))))
        (cond ((string-match-p anki-editor-cloze-opening-regexp matched)
               (setq depth (1+ depth)))
              ((string-match-p anki-editor-cloze-closing-regexp matched)
               (setq depth (1- depth)))
              ((string-match-p anki-editor-cloze-newline-regexp matched)
               (setq multiline t))
              (t (error "Unexpected structural match in cloze scanner")))
        (when (= depth 0)
          (throw :exit (cons (point) multiline)))))))

(defun anki-editor-cloze-nested-matcher (limit)
  "Font-lock search function matching cloze syntax up to LIMIT."
  (when (re-search-forward anki-editor-cloze-opening-regexp limit t)
    (let* ((opening-beg (match-beginning 1)) (opening-end (match-end 1))
           (label-beg (match-beginning 2)) (label-end (match-end 2))
           (colons-beg (match-beginning 3)) (colons-end (match-end 3))
           (payload-beg (point)) ; right after double-colon
           (label (buffer-substring-no-properties label-beg label-end))
           (label-num (string-to-number (substring label 1)))
           (props-invisible '( face font-lock-comment-face
                               invisible anki-editor-cloze-hide
                               rear-nonsticky (invisible) )))
      (save-excursion
        (pcase-let*
            ((`(,pt . ,ml) (anki-editor-cloze-find-balanced-end limit))
             (payload-end (- pt 2))
             (closing-beg (- pt 2)) (closing-end pt)
             (props-opening
              (if-let ((dspl (or (and ml anki-editor-cloze-multiline-payload-opening-display)
                                 (and (null ml) anki-editor-cloze-payload-opening-display))))
                  `( face anki-editor-cloze-paren-face
                     display ,dspl
                     rear-nonsticky (display) )
                props-invisible))
             (props-label
              (if-let ((_ (eq anki-editor-cloze-label-display-method 'inline))
                       (dspl (or (and ml anki-editor-cloze-multiline-label-display)
                                 (and (null ml) anki-editor-cloze-label-display))))
                  `( face anki-editor-cloze-label
                     display ,dspl
                     rear-nonsticky (display) )
                props-invisible))
             (props-colon props-invisible)
             (props-payload
              (append
               `( cursor-face ,(or (and ml 'anki-editor-cloze-multiline-payload-cursor-face)
                                   (anki-editor-cloze--pick-face anki-editor-cloze-payload-cursor-faces label-num)) )
               (when (eq anki-editor-cloze-label-display-method 'help-echo)
                 `( help-echo ,label ))))
             (props-closing
              (if-let ((dspl (or (and ml anki-editor-cloze-multiline-payload-closing-display)
                                 (and (not ml) anki-editor-cloze-payload-closing-display))))
                  `( face anki-editor-cloze-paren-face
                     display ,dspl
                     rear-nonsticky (display) )
                props-invisible)))
          (add-text-properties opening-beg opening-end props-opening)
          (add-text-properties label-beg label-end props-label)
          (add-text-properties colons-beg colons-end props-colon)
          (add-text-properties payload-beg payload-end props-payload)
          (add-face-text-property payload-beg payload-end
                                  (or (and ml 'anki-editor-cloze-multiline-payload-face)
                                      (anki-editor-cloze--pick-face anki-editor-cloze-payload-faces label-num))
                                  t)
          (add-text-properties closing-beg closing-end props-closing)
          (setq-local anki-editor-nested--last (cons opening-beg pt))
          t)))))

;;; Minor Mode Definition

;;;###autoload
(define-minor-mode anki-editor-cloze-mode
  "Minor mode to visually conceal and highlight Anki cloze deletions."
  :init-value nil
  :lighter " Cloze"
  :group 'anki-editor-cloze
  (if anki-editor-cloze-mode
      (progn
        (when (fboundp 'cursor-face-highlight-mode)
          (cursor-face-highlight-mode 1))
        (add-hook 'font-lock-extend-region-functions #'anki-editor-cloze-extend-region nil t)
        (add-to-invisibility-spec 'anki-editor-cloze-hide)
        (setq-local font-lock-extra-managed-props
                    (append '(display invisible help-echo cursor-face rear-nonsticky)
                            font-lock-extra-managed-props))
        (font-lock-add-keywords nil '((anki-editor-cloze-nested-matcher)) t)
        (font-lock-flush))
    (remove-from-invisibility-spec 'anki-editor-cloze-hide)
    (font-lock-remove-keywords nil '((anki-editor-cloze-nested-matcher)))
    (remove-hook 'font-lock-extend-region-functions #'anki-editor-cloze-extend-region t)
    (font-lock-flush)))

(provide 'anki-editor-cloze)

;;; anki-editor-cloze.el ends here
