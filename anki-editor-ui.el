;;; anki-editor-ui.el --- UI for anki-editor  -*- lexical-binding: t; -*-

;; Author: orgtre
;; Version: 0.3.3
;; URL: https://github.com/anki-editor/anki-editor
;; Package-Requires: ((emacs "28.1"))

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

;; An interactive UI for anki-editor, based on transient.

;;; Code:

(require 'transient)
(require 'anki-editor)

(defgroup anki-editor-ui nil
  "Customizations for anki-editor-ui."
  :group 'anki-editor)

(defcustom anki-editor-ui-match-preprompt
  "Syntax: [+-&|][tag|{tagregex}|property[=|<>|<|>|<=|>=]value]\n"
  "Extra syntax explanation shown before the match prompt."
  :type 'string)

(defcustom anki-editor-ui-deck-preprompt
  "Use 'TAB' to complete, ',' to select multiple, and 'RET' to finalize.\n"
  "Extra syntax explanation shown before the deck prompt."
  :type 'string)

(transient-define-prefix anki-editor-ui ()
  "Transient for anki-editor."
  [["Org"
    ("i" " insert default note" anki-editor-insert-default-note :level 1)
    ("t" " set default note type" anki-editor-set-default-note-type
     :level 1)
    ("c" " cloze dwim" anki-editor-cloze-dwim :level 2)]
   [""
    ("I" " insert note" anki-editor-insert-note :level 1)
    ("T" " set note type" anki-editor-set-note-type :level 1)
    ("d" " delete note" anki-editor-delete-note-at-point :level 1)]]
  [["Push"
    ("." " note at point        " anki-editor-push-note-at-point :level 2)
    ("n" " new notes            " anki-editor-push-new-notes :level 2)
    ("p" " push with ui" anki-editor-ui-push :level 1)]
   [""
    ("b" " notes in buffer" anki-editor-push-notes :level 2)
    ("f" " failed notes" anki-editor-retry-failed-notes :level 2)
    ""]]
  [["Anki"
    ("g" " gui browse           " anki-editor-gui-browse :level 2)
    ("s" " sync AnkiWeb         " anki-editor-sync-collection :level 2)]
   [""
    ("G" " gui add card" anki-editor-gui-add-cards :level 3)
    ("a" " api check" anki-editor-api-check :level 3)]]
  [["Misc"
    ("h" " subtree to html      " anki-editor-export-subtree-to-html
     :level 4)
    ("C" " copy styles          " anki-editor-copy-styles :level 4)
    ("F" " toggle format        " anki-editor-toggle-format :level 4)]
   [""
    ("H" " region to html" anki-editor-convert-region-to-html :level 4)
    ("R" " remove styles" anki-editor-remove-styles :level 4)
    ("P" " toggle prepend heading" anki-editor-toggle-prepend-heading
     :level 4)]])

(transient-define-prefix anki-editor-ui-push ()
  "Transient for pushing anki-editor notes."
  :incompatible '(("new" "failed" "existing"))
  ["Push which notes?"
   ("n" " new" "new")
   ("f" " failed" "failed")
   ("e" " existing" "existing")
   ("t" " note-type" "note-type=" anki-editor-ui--read-note-type)
   ("d" " decks" "decks=" anki-editor-ui--read-decks :multi-value rest)
   ("m" " match" "match=" anki-editor-ui--read-match)]
  ["From where?"
   ("." " point" anki-editor-push-note-at-point)
   ("r" " active region" anki-editor-ui-push-region :if region-active-p)
   ("s" " subtree" anki-editor-ui-push-subtree)
   ("b" " narrowed buffer" anki-editor-ui-push-narrowed-buffer)
   ("B" " full buffer" anki-editor-ui-push-full-buffer)
   ("A" " agenda files" anki-editor-ui-push-agenda-files)])

(defun anki-editor-ui--read-note-type (prompt initial-input history)
  (completing-read prompt (anki-editor-note-types)
		   nil nil initial-input history))

(defun anki-editor-ui--read-decks (prompt initial-input history)
  (completing-read-multiple (concat anki-editor-ui-deck-preprompt prompt)
			    (anki-editor-deck-names)
			    nil nil initial-input history))

(defun anki-editor-ui--read-match (prompt initial-input history)
  (read-string (concat anki-editor-ui-match-preprompt prompt)
	       initial-input history))

(defun anki-editor-ui-push-region (&optional args)
  "Used by `anki-editor-ui-push' to push region with ARGS."
  (interactive (list (transient-args transient-current-command)))
  (anki-editor-ui-push--pass-args-and-push args 'region))

(defun anki-editor-ui-push-subtree (&optional args)
  "Used by `anki-editor-ui-push' to push subtree with ARGS."
  (interactive (list (transient-args transient-current-command)))
  (anki-editor-ui-push--pass-args-and-push args 'tree))

(defun anki-editor-ui-push-narrowed-buffer (&optional args)
  "Used by `anki-editor-ui-push' to push narrowed buffer with ARGS."
  (interactive (list (transient-args transient-current-command)))
  (anki-editor-ui-push--pass-args-and-push args nil))

(defun anki-editor-ui-push-full-buffer (&optional args)
  "Used by `anki-editor-ui-push' to push full buffer with ARGS."
  (interactive (list (transient-args transient-current-command)))
  (anki-editor-ui-push--pass-args-and-push args 'file))

(defun anki-editor-ui-push-agenda-files (&optional args)
  "Used by `anki-editor-ui-push' to push agenda files with ARGS."
  (interactive (list (transient-args transient-current-command)))
  (anki-editor-ui-push--pass-args-and-push args 'agenda))

(defun anki-editor-ui-push--pass-args-and-push (args scope)
  "Pass the `transient-args` ARGS to `anki-editor-push-notes`.
Also pass SCOPE."
  (let-alist (anki-editor-ui-push--parse-args args)
    (anki-editor-push-notes
     scope .fullmatch
     (when .decks `(apply #'anki-editor-ui--skip-unless-decks
			  (quote ,.decks))))))

(defun anki-editor-ui-push--parse-args (args)
  "Parse the `transient-args` ARGS from `anki-editor-push`.
Return an alist with the full match pattern and deck."
  (let ((new (transient-arg-value "new" args))
	(failed (transient-arg-value "failed" args))
	(existing (transient-arg-value "existing" args))
	(note-type (transient-arg-value "note-type=" args))
	(decks (alist-get "decks=" args nil nil 'equal))
	(match (transient-arg-value "match=" args)))
    (let ((fullmatch
	   (concat
	    (when new
	      (concat "+" anki-editor-prop-note-id "=\"\""))
	    (when failed
	      (concat "+" anki-editor-prop-failure-reason "<>\"\""))
	    (when existing
	      (concat "+" anki-editor-prop-note-id "<>\"\""))
	    (when note-type
	      (concat "+" anki-editor-prop-note-type "=\"" note-type "\""))
	    ;; note that the match syntax doesn't allows us to specify
	    ;; several alternative note types here
	    (when match
	      (if (string-match "^[-+&|/]" match)
		  match
		(concat "+" match))))))
      (list (cons 'fullmatch fullmatch)(cons 'decks decks)))))

(defun anki-editor-ui--skip-unless-decks (&rest filter-decks)
  "Skip function passed to `org-map-entries`.
A note (subtree) is skipped unless its deck is in FILTER-DECKS.
We can't just filter by deck using `org-map-entries` match argument,
since we need to turn off property inheritance when mapping notes."
  (let ((deck (org-entry-get nil anki-editor-prop-deck t)))
    (unless (member deck filter-decks)
      (save-excursion
        (org-end-of-subtree t)
        (point)))))

(provide 'anki-editor-ui)

;;; anki-editor-ui.el ends here
