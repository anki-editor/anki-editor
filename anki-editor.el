;;; anki-editor.el --- Minor mode for making Anki cards with Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022 Lei Tan <louietanlei[at]gmail[dot]com>

;; Author: Lei Tan
;; Version: 0.3.3
;; URL: https://github.com/louietan/anki-editor
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

;;; Commentary:
;;
;;  This package is for users of both Emacs and Anki, who'd like to
;;  make Anki cards in Org mode.  With this package, Anki cards can be
;;  made from an Org buffer like below (inspired by org-drill):
;;
;;  * Sample                  :emacs:lisp:programming:
;;    :PROPERTIES:
;;    :ANKI_DECK: Computing
;;    :ANKI_NOTE_TYPE: Basic
;;    :END:
;;  ** Front
;;     How to say "hello world" in elisp?
;;  ** Back
;;     #+BEGIN_SRC emacs-lisp
;;       (message "Hello, world!")
;;     #+END_SRC
;;
;;  This package extends Org-mode's built-in HTML backend to generate
;;  HTML for contents of note fields with specific syntax (e.g. latex)
;;  translated to Anki style.
;;
;;  For this package to work, you have to setup these external dependencies:
;;  - Anki
;;  - AnkiConnect, an Anki addon that runs an RPC server over HTTP to expose
;;                 Anki functions as APIs, for installation instructions see
;;                 https://github.com/FooSoft/anki-connect#installation
;;  - curl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org-element)
(require 'ox)
(require 'ox-html)

(defgroup anki-editor nil
  "Customizations for anki-editor."
  :group 'org)

(defcustom anki-editor-break-consecutive-braces-in-latex nil
  "If non-nil, automatically separate consecutive `}' in latex by spaces.
This prevents early closing of cloze."
  :type 'boolean)

(defcustom anki-editor-org-tags-as-anki-tags t
  "If nil, tags of entries won't be counted as Anki tags."
  :type 'boolean)

(defcustom anki-editor-protected-tags '("marked" "leech")
  "A list of protected tags to not delete from Anki.
These won't be deleted from Anki even when they're absent in Org entries.
Useful for special tags like `marked' and `leech'."
  :type '(repeat string))

(defcustom anki-editor-ignored-org-tags
  (append org-export-select-tags org-export-exclude-tags)
  "A list of Org tags that are ignored when constructing notes."
  :type '(repeat string))

(defcustom anki-editor-api-host "127.0.0.1"
  "The network address AnkiConnect is listening on."
  :type 'string)

(defcustom anki-editor-api-port "8765"
  "The port number AnkiConnect is listening on."
  :type 'string)

(defcustom anki-editor-latex-style 'builtin
  "The style of latex to translate into."
  :type '(radio (const :tag "Built-in" builtin)
                (const :tag "MathJax" mathjax)))

(defcustom anki-editor-include-default-style t
  "Whether to include the default style with `anki-editor-copy-styles'.
The default style is specified in `org-html-style-default'.
For example, you might want to turn this off when you are going to
provide your custom styles in `anki-editor-html-head'."
  :type 'boolean)

(defcustom anki-editor-html-head nil
  "Additional html tags to append with `anki-editor-copy-styles'.
Can be used to add custom styles and scripts to card styles."
  :type 'string)

(defcustom anki-editor-note-match nil
  "Additional matching string for mapping through Anki note headings.
A leading logical operator like `+' or `&' is required."
  :type 'string)

(defcustom anki-editor-prepend-heading nil
  "Prepend note heading to contents before first (sub)heading.
This is only in effect when exactly one note-type field is not found
among the note subheadings and there is content before the first subheading."
  :type 'boolean)

(defcustom anki-editor-prepend-heading-format "/%s/\n\n"
  "Format string used when prepending note heading.
It should contain `%s' as placeholder for the heading, see `format'."
  :type 'string)

(defcustom anki-editor-insert-note-always-use-content nil
  "Whether to always make use of content before (sub)heading.
See `anki-editor-insert-note', whose behavior this controls."
  :type 'boolean)

(defcustom anki-editor-default-note-type "Basic"
  "Default note type when creating anki-editor notes in org.
Only used when no ANKI_DEFAULT_NOTE_TYPE property is inherited."
  :type 'string)

(defcustom anki-editor-gui-browse-ensure-foreground t
  "Ensure that `anki-editor-gui-browse' opens in foreground."
  :type 'boolean)


;;; AnkiConnect

(defconst anki-editor-api-version 6)

(cl-defun anki-editor--fetch (url
                              &rest settings
                              &key
                              (type "GET")
                              data success _error
                              (parser 'buffer-string)
                              &allow-other-keys)
  "Fetch URL using curl.
The api is borrowed from request.el."
;; This exists because request.el's sync mode calls curl asynchronously under
;; the hood, which doesn't work on some machines (like mine) where the process
;; sentinel never gets called. After some debugging of Emacs, it seems that in
;; 'process.c' the pselect syscall to the file descriptor of inotify used by
;; 'autorevert' always returns a nonzero value and causes 'status_notify' never
;; being called. To determine whether it's a bug in Emacs and make a patch
;; requires more digging.
  (let ((tempfile (make-temp-file "emacs-anki-editor"))
        (responsebuf (generate-new-buffer " *anki-editor-curl*")))
    (when data
      (with-temp-file tempfile
        (setq buffer-file-coding-system 'utf-8)
        (set-buffer-multibyte t)
        (insert data)))
    (unwind-protect
        (with-current-buffer responsebuf
          (apply #'call-process "curl" nil t nil (list
                                                  url
                                                  "--silent"
                                                  "-X" type
                                                  "--data-binary"
                                                  (concat "@" tempfile)))

          (goto-char (point-min))
          (when success
            (apply success (list :data (funcall parser)))))
      (kill-buffer responsebuf)
      (delete-file tempfile))))

(defun anki-editor-api-call (action &rest params)
  "Invoke AnkiConnect with ACTION and PARAMS."
  (let ((payload (list :action action :version anki-editor-api-version))
        (_request-backend 'curl)
        (json-array-type 'list)
        reply err)

    (when params
      (plist-put payload :params params))

    (anki-editor--fetch (format "http://%s:%s"
                                anki-editor-api-host
                                anki-editor-api-port)
			:type "POST"
                        :parser 'json-read
                        :data (json-encode payload)
                        :success (cl-function
				  (lambda (&key data &allow-other-keys)
                                    (setq reply data)))
                        :error (cl-function
				(lambda (&key error-thrown &allow-other-keys)
                                  (setq err (string-trim (cdr error-thrown)))))
                        :sync t)
    (when err
      (error "Error communicating with AnkiConnect using cURL: %s" err))
    (or reply (error "Got empty reply from AnkiConnect"))))

(defun anki-editor-api-call-result (&rest args)
  "Invoke AnkiConnect with ARGS and return the result from response.
Raise an error if applicable."
  (let-alist (apply #'anki-editor-api-call args)
    (when .error (error .error))
    .result))

(defmacro anki-editor-api-with-multi (&rest body)
  "Use in combination with `anki-editor-api-enqueue' to combine
multiple api calls into a single 'multi' call, return the results
of these calls in the same order."
  `(let (--anki-editor-var-multi-actions--
         --anki-editor-var-multi-results--)
     ,@body
     (setq --anki-editor-var-multi-results--
           (anki-editor-api-call-result
            'multi
            :actions (nreverse
                      ;; Here we make a vector from the action list,
                      ;; or `json-encode' will consider it as an alist.
                      (vconcat
                       --anki-editor-var-multi-actions--))))
     (cl-loop for result in --anki-editor-var-multi-results--
              do (when-let ((pred (listp result))
                            (err (alist-get 'error result)))
                   (error err))
              collect result)))

(defmacro anki-editor-api-enqueue (action &rest params)
  "Like `anki-editor-api-call', but is only used in combination
with `anki-editor-api-with-multi'.  Instead of sending the
request directly, it simply queues the request."
  `(let ((action (list :action ,action))
         (params (list ,@params)))
     (when params
       (plist-put action :params params))
     (push action --anki-editor-var-multi-actions--)))

(defun anki-editor-api--note (note)
  "Convert NOTE to the form that AnkiConnect accepts."
  (list
   :id (string-to-number (or (anki-editor-note-id note) "0"))
   :deckName (anki-editor-note-deck note)
   :modelName (anki-editor-note-model note)
   :fields (anki-editor-note-fields note)
   ;; Convert tags to a vector since empty list is identical to nil
   ;; which will become None in Python, but AnkiConnect requires it
   ;; to be type of list.
   :tags (vconcat (anki-editor-note-tags note))))

(defun anki-editor-api--store-media-file (path)
  "Store media file for PATH, which is an absolute file name.
The result is the path to the newly stored media file."
  (let* ((bytes (with-temp-buffer
                  (insert-file-contents-literally path)
                  (buffer-string)))
         (hash (secure-hash 'sha1 bytes))
         (media-file-name (format "%s-%s%s"
                                  (file-name-base path)
                                  hash
                                  (file-name-extension path t))))
    (when (eq :json-false
              (anki-editor-api-call-result 'retrieveMediaFile
                                           :filename media-file-name))
      (message "Storing media file %s to Anki, this might take a while" path)
      (anki-editor-api-call-result 'storeMediaFile
                                   :filename media-file-name
                                   :data (base64-encode-string bytes)))
    media-file-name))


;;; Org export backend

(defconst anki-editor--ox-anki-html-backend
  (org-export-create-backend
   :parent 'html
   :transcoders '((latex-fragment . anki-editor--ox-latex)
                  (latex-environment . anki-editor--ox-latex))))

(defconst anki-editor--ox-export-ext-plist
  '(:with-toc nil :with-properties nil :with-planning nil :anki-editor-mode t))

(defconst anki-editor--audio-extensions
  '(".mp3" ".3gp" ".flac" ".m4a" ".oga" ".ogg" ".opus" ".spx" ".wav"))

(cl-macrolet
    ((with-table (table)
                 `(cl-loop for delims in ,table
                           collect
                           (list (concat "^" (regexp-quote (cl-first delims)))
                                 (cl-second delims)
                                 (concat (regexp-quote (cl-third delims)) "$")
                                 (cl-fourth delims)))))

  (defconst anki-editor--native-latex-delimiters
    (with-table '(("$$" "[$$]"
                   "$$" "[/$$]")
                  ("$" "[$]"
                   "$" "[/$]")
                  ("\\(" "[$]"
                   "\\)" "[/$]")
                  ("\\[" "[$$]"
                   "\\]" "[/$$]"))))

  (defconst anki-editor--mathjax-delimiters
    (with-table '(("$$" "\\["
                   "$$" "\\]")
                  ("$" "\\("
                   "$" "\\)")))))

(defun anki-editor--translate-latex-fragment (latex-code)
  "Translate LATEX-CODE fragment to html."
  (cl-loop for delims in (cl-ecase anki-editor-latex-style
                           (builtin anki-editor--native-latex-delimiters)
                           (mathjax anki-editor--mathjax-delimiters))
           for matches = (string-match (cl-first delims) latex-code)
           when matches
           do
           (setq latex-code (replace-match (cl-second delims) t t latex-code))
           (string-match (cl-third delims) latex-code)
           (setq latex-code (replace-match (cl-fourth delims) t t latex-code))
           until matches
           finally return latex-code))

(defun anki-editor--translate-latex-env (latex-code)
  "Translate LATEX-CODE environment to html."
  (setq latex-code (replace-regexp-in-string
		    "\n" "<br>" (org-html-encode-plain-text latex-code)))
  (cl-ecase anki-editor-latex-style
    (builtin (concat "[latex]<br>" latex-code "[/latex]"))
    (mathjax (concat "\\[<br>" latex-code "\\]"))))

(defun anki-editor--ox-latex (latex _contents _info)
  "Transcode LATEX from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((code (org-remove-indentation (org-element-property :value latex))))
    (setq code (cl-ecase (org-element-type latex)
                 (latex-fragment (anki-editor--translate-latex-fragment code))
                 (latex-environment (anki-editor--translate-latex-env code))))
    (if anki-editor-break-consecutive-braces-in-latex
        (replace-regexp-in-string "}}" "} } " code)
      code)))

(defun anki-editor--ox-html-link (oldfun link desc info)
  "Export LINK and its target.
When LINK is a link to local file, transcode it to html
and store the target file to Anki, otherwise call OLDFUN for help.
The implementation is borrowed and simplified from ox-html."
  (or
   (catch 'giveup
     (unless (plist-get info :anki-editor-mode)
       (throw 'giveup nil))


     (let* ((type (org-element-property :type link))
            (raw-path (org-element-property :path link))
            (desc (org-string-nw-p desc))
            (path
             (cond
              ((string= type "file")
               ;; Possibly append `:html-link-home' to relative file
               ;; name.
               (let ((inhibit-message nil)
                     (home (and (plist-get info :html-link-home)
                                (org-trim (plist-get info :html-link-home)))))
                 (when (and home
                            (plist-get info :html-link-use-abs-url)
                            (file-name-absolute-p raw-path))
                   (setq raw-path
			 (concat (file-name-as-directory home) raw-path)))
                 ;; storing file to Anki and return the modified path
                 (anki-editor-api--store-media-file
		  (expand-file-name (url-unhex-string raw-path)))))
              (t (throw 'giveup nil))))
            (attributes-plist
             (let* ((parent (org-export-get-parent-element link))
                    (link (let ((container (org-export-get-parent link)))
                            (if (and (eq (org-element-type container) 'link)
                                     (org-html-inline-image-p link info))
                                container
                              link))))
               (and (eq (org-element-map parent 'link 'identity info t) link)
                    (org-export-read-attribute :attr_html parent))))
            (attributes
             (let ((attr (org-html--make-attribute-string attributes-plist)))
               (if (org-string-nw-p attr) (concat " " attr) ""))))
       (cond
        ;; Image file.
        ((and (plist-get info :html-inline-images)
              (org-export-inline-image-p
               link (plist-get info :html-inline-image-rules)))
         (org-html--format-image path attributes-plist info))

        ;; Audio file.
	((cl-some (lambda (string) (string-suffix-p string path t))
		  anki-editor--audio-extensions)
	 (format "[sound:%s]" path))

        ;; External link with a description part.
        ((and path desc) (format "<a href=\"%s\"%s>%s</a>"
                                 (org-html-encode-plain-text path)
                                 attributes
                                 desc))

        ;; External link without a description part.
        (path (let ((path (org-html-encode-plain-text path)))
                (format "<a href=\"%s\"%s>%s</a>"
                        path
                        attributes
                        (org-link-unescape path))))

        (t (throw 'giveup nil)))))
   (funcall oldfun link desc info)))

(defun anki-editor--export-field (src fmt)
  "Export field as string if SRC is a string, otherwise export it as a region.

If FMT is non-nil, format the exported string."
  (if (stringp src)
      (anki-editor--export-string src fmt)
    (anki-editor--export-region src fmt)))


(defun anki-editor--export-region (src fmt)
  "Export region SRC and format it if FMT."
  (cl-ecase fmt
    ('nil (buffer-substring-no-properties (car src) (car (cdr src))))
    ('t (save-window-excursion
          (save-mark-and-excursion
            (set-mark (car src))
            (goto-char (car (cdr src)))
            (or (org-export-as anki-editor--ox-anki-html-backend
                               t
                               nil
                               t
                               anki-editor--ox-export-ext-plist)
            ""))))))

(defun anki-editor--export-string (src fmt)
  "Export string SRC and format it if FMT."
  (cl-ecase fmt
    ('nil src)
    ('t (or (org-export-string-as src
                                  anki-editor--ox-anki-html-backend
                                  t
                                  anki-editor--ox-export-ext-plist)
            ;; 8.2.10 version of
            ;; `org-export-filter-apply-functions'
            ;; returns nil for an input of empty string,
            ;; which will cause AnkiConnect to fail
            ""))))


;;; Core primitives

(defconst anki-editor-prop-note-type "ANKI_NOTE_TYPE")
(defconst anki-editor-prop-note-id "ANKI_NOTE_ID")
(defconst anki-editor-prop-deck "ANKI_DECK")
(defconst anki-editor-prop-format "ANKI_FORMAT"
  "Format subheading and content using html backend.")
(defconst anki-editor-prop-prepend-heading "ANKI_PREPEND_HEADING")
(defconst anki-editor-prop-field-prefix "ANKI_FIELD_"
  "Anki fields with names got from an org-node property.")
(defconst anki-editor-prop-tags "ANKI_TAGS")
(defconst anki-editor-prop-tags-plus (concat anki-editor-prop-tags "+"))
(defconst anki-editor-prop-failure-reason "ANKI_FAILURE_REASON")
(defconst anki-editor-prop-default-note-type "ANKI_DEFAULT_NOTE_TYPE")
(defconst anki-editor-org-tag-regexp "^\\([[:alnum:]_@#%]+\\)+$")

(cl-defstruct anki-editor-note
  id model deck fields tags)

(defvar anki-editor--collection-data-updated nil
  "Whether or not collection data is updated from Anki.
Used by `anki-editor--with-collection-data-updated'
to avoid unnecessary updates.")

;; The following variables should only be used inside
;; `anki-editor--with-collection-data-updated'.

(defvar anki-editor--model-names nil
  "Note types from Anki.")

(defvar anki-editor--model-fields nil
  "Alist of (NOTE-TYPE . FIELDS).")

(defmacro anki-editor--with-collection-data-updated (&rest body)
  "Execute BODY with collection data updated from Anki.
Note that since we have no idea of whether BODY will update collection
data, BODY might read out-dated data.  This doesn't matter right now
as note types won't change in BODY."
  (declare (indent defun) (debug t))
  `(if anki-editor--collection-data-updated
       (progn ,@body)
     (cl-destructuring-bind (models)
         (anki-editor-api-with-multi
          (anki-editor-api-enqueue 'modelNames))
       (unwind-protect
           (progn
             (setq anki-editor--collection-data-updated t
                   anki-editor--model-names models
                   anki-editor--model-fields
                   (cl-loop
		    for flds in (eval `(anki-editor-api-with-multi
                                        ,@(cl-loop
					   for mod in models
                                           collect `(anki-editor-api-enqueue
						     'modelFieldNames
						     :modelName ,mod))))
                    for mod in models
                    collect (cons mod flds)))
             ,@body)
         (setq anki-editor--collection-data-updated nil)))))

(defun anki-editor-map-note-entries (func &optional match scope &rest skip)
  "Apply FUNC to each anki-editor note matching MATCH in SCOPE.
Simple wrapper that calls `org-map-entries' with entries that match
`ANKI_NOTE_TYPE<>\"\"', `anki-editor-note-match' and MATCH.
A leading logical operator like `+' or `&' is required in MATCH."
  ;; disable property inheritance temporarily, or all subheadings of a
  ;; note heading will be counted as note headings as well
  (let ((org-use-property-inheritance nil))
    (apply #'org-map-entries
	   func
	   (concat "+" anki-editor-prop-note-type "<>\"\""
		   match
		   anki-editor-note-match)
	   scope
	   skip)))

(defun anki-editor--insert-note-skeleton (prefix deck heading type fields)
  "Insert a note subtree (skeleton) with HEADING, TYPE and FIELDS.
DECK is only inserted if not already inherited. For PREFIX and more
see `anki-editor-insert-note' which wraps this function."
  (org-insert-heading-respect-content)
  (insert heading)
  (org-set-property anki-editor-prop-note-type type)
  (unless (save-excursion
            (org-up-heading-safe)
            (and (not (string-blank-p deck))
                 (string= deck (org-entry-get-with-inheritance
				anki-editor-prop-deck))))
    (org-set-property anki-editor-prop-deck deck))
  (when (string-blank-p heading)
    (setq fields (cdr fields)))
  (when (equal 1 (length fields))
    (setq fields nil))
  (let ((use-content (if prefix
			 (not anki-editor-insert-note-always-use-content)
		       anki-editor-insert-note-always-use-content)))
    (when use-content
      (setq fields (cdr fields)))
    (dolist (field fields)
      (save-excursion
	(org-insert-heading-respect-content)
	(org-do-demote)
	(insert field)))
    (when (and (not (string-blank-p heading))
	       (not use-content))
      (org-goto-first-child)
      (end-of-line))))

(defun anki-editor--push-note (note)
  "Request AnkiConnect for updating or creating NOTE."
  (cond
   ((null (anki-editor-note-id note))
    (anki-editor--create-note note))
   (t
    (anki-editor--update-note note))))

(defun anki-editor--set-note-id (id)
  "Set note-id of anki-editor note at point to ID."
  (unless id
    (error "Note creation failed for unknown reason"))
  (org-set-property anki-editor-prop-note-id (number-to-string id)))

(defun anki-editor--create-note (note)
  "Request AnkiConnect for creating NOTE."
  (thread-last
    (anki-editor-api-with-multi
     (anki-editor-api-enqueue 'createDeck
                              :deck (anki-editor-note-deck note))
     (anki-editor-api-enqueue 'addNote
                              :note (anki-editor-api--note note)))
    (nth 1)
    (anki-editor--set-note-id)))

(defun anki-editor--update-note (note)
  "Request AnkiConnect for updating fields, deck, and tags of NOTE."
  (let* ((oldnote (caar (anki-editor-api-with-multi
                         (anki-editor-api-enqueue
			  'notesInfo
                          :notes (list (string-to-number
					(anki-editor-note-id note))))
                         (anki-editor-api-enqueue
			  'updateNoteFields
                          :note (anki-editor-api--note note)))))
         (tagsadd (cl-set-difference (anki-editor-note-tags note)
                                     (alist-get 'tags oldnote)
                                     :test 'string=))
         (tagsdel (thread-first (alist-get 'tags oldnote)
				(cl-set-difference (anki-editor-note-tags note)
						   :test 'string=)
				(cl-set-difference anki-editor-protected-tags
						   :test 'string=))))
    (anki-editor-api-with-multi
     (anki-editor-api-enqueue 'changeDeck
			      :cards (alist-get 'cards oldnote)
			      :deck (anki-editor-note-deck note))

     (when tagsadd
       (anki-editor-api-enqueue 'addTags
                                :notes (list (string-to-number
					      (anki-editor-note-id note)))
                                :tags (mapconcat #'identity tagsadd " ")))
     (when tagsdel
       (anki-editor-api-enqueue 'removeTags
                                :notes (list (string-to-number
					      (anki-editor-note-id note)))
                                :tags (mapconcat #'identity tagsdel " "))))))

(defun anki-editor--set-failure-reason (reason)
  "Set failure reason to REASON in property drawer at point."
  (org-entry-put nil anki-editor-prop-failure-reason reason))

(defun anki-editor--clear-failure-reason ()
  "Clear failure reason in property drawer at point."
  (org-entry-delete nil anki-editor-prop-failure-reason))

(defun anki-editor--get-allowed-values-for-property (property)
  "Get allowed values for PROPERTY."
  (pcase property
    ((pred (string= anki-editor-prop-deck)) (anki-editor-deck-names))
    ((pred (string= anki-editor-prop-note-type)) (anki-editor-note-types))
    ((pred (string= anki-editor-prop-format)) (list "t" "nil"))
    ((pred (string= anki-editor-prop-prepend-heading)) (list "t" "nil"))
    ((pred (string-match-p (format "%s\\+?" anki-editor-prop-tags)))
     (anki-editor-all-tags))
    ((pred (string= anki-editor-prop-default-note-type))
     (anki-editor-note-types))
    (_ nil)))

(defun anki-editor-is-valid-org-tag (tag)
  "Check if string TAG can be used as an Org tag."
  (string-match-p anki-editor-org-tag-regexp tag))

(defun anki-editor-all-tags ()
  "Get all tags from Anki."
  (anki-editor-api-call-result 'getTags))

(defun anki-editor-deck-names ()
  "Get all decks names from Anki."
  (anki-editor-api-call-result 'deckNames))

(defun anki-editor--enable-tag-completion ()
  (and anki-editor-mode anki-editor-org-tags-as-anki-tags))

(defun anki-editor--before-set-tags (&optional _ just-align)
  "Fetch and cache tags from Anki."
  (when (and (anki-editor--enable-tag-completion)
             (not just-align))
    (setq anki-editor--anki-tags-cache (anki-editor-all-tags))
    (when (cl-notevery #'anki-editor-is-valid-org-tag
		       anki-editor--anki-tags-cache)
      (warn (concat "Some tags from Anki contain characters that are not"
		    "valid in Org tags.")))))

(defun anki-editor--get-buffer-tags (oldfun)
  "Append tags from Anki to the result of applying OLDFUN."
  (append (funcall oldfun)
          (when (anki-editor--enable-tag-completion)
            (mapcar #'list anki-editor--anki-tags-cache))))

(defun anki-editor-note-types ()
  "Get note types from Anki."
  (anki-editor-api-call-result 'modelNames))

(defun anki-editor-entry-format ()
  (read (or (org-entry-get-with-inheritance anki-editor-prop-format t) "t")))

(defun anki-editor-toggle-format ()
  "Toggle format setting for entry at point.
Only set a property value at entry if necessary for toggling.
The setting is read with inheritance from the property
`anki-editor-prop-format' if it exists, and is t else."
  (interactive)
  (let ((initial-val (anki-editor-entry-format))
	(entry-val (org-entry-get nil anki-editor-prop-format nil t))
	inherited-val)
    (when entry-val
      (org-entry-delete nil anki-editor-prop-format)
      (setq inherited-val (anki-editor-entry-format)))
    (when (not entry-val)
      (setq inherited-val initial-val))
    (unless (equal inherited-val (not initial-val))
      (org-entry-put nil anki-editor-prop-format
		     (symbol-name (not initial-val))))))

(defun anki-editor-prepend-heading ()
  "Get prepend-heading setting for entry at point.
The setting is read with inheritance from the property
`anki-editor-prop-prepend-heading' if it exists,
and else from variable `anki-editor-prepend-heading'."
  (let ((p (or (org-entry-get-with-inheritance
                anki-editor-prop-prepend-heading t)
               anki-editor-prepend-heading
               nil)))
    (if (symbolp p) p (intern p))))

(defun anki-editor-toggle-prepend-heading ()
  "Toggle prepend-heading setting for entry at point.
Only set a property value at entry if necessary for toggling.
The setting is read with inheritance from the property
`anki-editor-prop-prepend-heading' if it exists,
and else from variable `anki-editor-prepend-heading'."
  (interactive)
  (let ((initial-val (anki-editor-prepend-heading))
	(entry-val (org-entry-get nil anki-editor-prop-prepend-heading nil t))
	inherited-val)
    (when entry-val
      (org-entry-delete nil anki-editor-prop-prepend-heading)
      (setq inherited-val (anki-editor-prepend-heading)))
    (when (not entry-val)
      (setq inherited-val initial-val))
    (unless (equal inherited-val (not initial-val))
      (org-entry-put nil anki-editor-prop-prepend-heading
		     (symbol-name (not initial-val))))))

(defun anki-editor-note-at-point ()
  "Make a note struct from current entry."
  (let* ((deck (org-entry-get-with-inheritance anki-editor-prop-deck))
         (format (anki-editor-entry-format))
	     (prepend-heading (anki-editor-prepend-heading))
         (note-id (org-entry-get nil anki-editor-prop-note-id))
         (note-type (org-entry-get nil anki-editor-prop-note-type))
         (tags (cl-set-difference (anki-editor--get-tags)
                                  anki-editor-ignored-org-tags
                                  :test #'string=))
	     (heading (substring-no-properties (org-get-heading t t t t)))
	     (level (org-current-level))
	     (content-before-subheading
	      (anki-editor--note-contents-before-subheading))
	     (subheading-fields (anki-editor--build-fields))
         (fields (anki-editor--map-fields heading
					                      content-before-subheading
					                      subheading-fields
					                      note-type
					                      level
					                      prepend-heading
                                          format)))
    (unless deck (user-error "Missing deck"))
    (unless note-type (user-error "Missing note type"))
    (make-anki-editor-note :id note-id
                           :model note-type
                           :deck deck
                           :tags tags
                           :fields (sort fields (lambda (left right) (string< (car left) (car right)))))))

(defun anki-editor--get-tags ()
  "Return list of tags of org entry at point."
  (let ((tags (anki-editor--entry-get-multivalued-property-with-inheritance
               nil
               anki-editor-prop-tags)))
    (if anki-editor-org-tags-as-anki-tags
        (append tags (org-get-tags))
      tags)))

(defun anki-editor--entry-get-multivalued-property-with-inheritance (pom
								     property)
  "Return a list of values in a multivalued property with inheritance."
  (let* ((value (org-entry-get pom property t))
	 (values (and value (split-string value))))
    (mapcar #'org-entry-restore-space values)))


(defun anki-editor--build-fields ()
  "Build a list of fields from subheadings of current heading.

Return a list of cons of (FIELD-NAME . FIELD-CONTENT)."
  (save-excursion
    (cl-loop with inhibit-message = t
	     ;; suppress echo message from `org-babel-exp-src-block'
             initially (unless (org-goto-first-child)
                         (cl-return))
             for last-pt = (point)
             for element = (org-element-at-point)
             for heading = (substring-no-properties
                            (org-element-property :raw-value element))
             ;; contents-begin includes drawers and scheduling data,
             ;; which we'd like to ignore, here we skip these
             ;; elements and reset contents-begin.
             for begin = (save-excursion
			   (cl-loop for eoh = (org-element-property
					       :contents-begin element)
                                    then (org-element-property :end subelem)
				    while eoh
                                    for subelem = (progn
                                                    (goto-char eoh)
                                                    (org-element-context))
                                    while (memq
					   (org-element-type subelem)
					   '(drawer planning property-drawer))
                                    finally return (and eoh
							(org-element-property
							 :begin subelem))))
             for end = (org-element-property :contents-end element)
             for raw = (or (and begin
                                end
                                (list begin
                                      ;; in case the buffer is narrowed,
                                      ;; e.g. by `org-map-entries' when
                                      ;; scope is `tree'
                                      (min (point-max) end)))
                           "")
             ;; for content = (anki-editor--export-string raw format)
             ;; collect (cons heading content)
	     collect (cons heading raw)
             ;; proceed to next field entry and check last-pt to
             ;; see if it's already the last entry
             do (org-forward-heading-same-level nil t)
             until (= last-pt (point)))))

(defun anki-editor--property-fields (fields)
  "Extract Anki FIELDS from entry properties."
  (cl-loop for field in fields
           for property = (concat anki-editor-prop-field-prefix (upcase field))
           for property-value = (org-entry-get-with-inheritance property)
           when property-value
           collect (cons field property-value)))

(defun anki-editor--note-contents-before-subheading ()
  "Get content between heading at point and next sub/heading.

Leading whitespace, drawers, and planning content is skipped."
  (save-excursion
    (let* ((element (org-element-at-point))
	   (begin (cl-loop for eoh = (org-element-property
				      :contents-begin element)
			   then (org-element-property :end subelem)
			   while eoh
			   for subelem = (progn
					   (goto-char eoh)
					   (org-element-context))
			   while (memq (org-element-type subelem)
				       '(drawer planning property-drawer))
			   finally return (and eoh (org-element-property
						    :begin subelem))))
	   (end (cl-loop for eoh = (org-element-property
				    :contents-begin element)
			 then (org-element-property :end nextelem)
			 while eoh
			 for nextelem = (progn
					  (goto-char eoh)
					  (org-element-at-point))
			 while (not (memq (org-element-type nextelem)
					  '(headline)))
			 finally return (and eoh (org-element-property
						  :begin nextelem))))
	   (contents-raw (or (and begin
				              end
                              (list begin
				                    ;; in case the buffer is narrowed,
				                    ;; e.g. by `org-map-entries' when
				                    ;; scope is `tree'
				                    (min (point-max) end)))
			             "")))
      contents-raw)))


(defun anki-editor--map-fields (heading
				                content-before-subheading
				                subheading-fields
				                note-type
				                level
				                prepend-heading
                                format)
  "Map `heading', pre-subheading content, and subheadings to fields.

When the `subheading-fields' don't match the `note-type's fields,
map missing fields to the `heading' and/or `content-before-subheading'.
Return a list of cons of (FIELD-NAME . FIELD-CONTENT)."
  (anki-editor--with-collection-data-updated
    (let* ((model-fields (alist-get
                          note-type anki-editor--model-fields
                          nil nil #'string=))  ; Fields Anki wants to be in the note
           (property-fields (anki-editor--property-fields model-fields))  ; Fields defined in the property
           (named-fields (seq-uniq (append subheading-fields property-fields)
                                   (lambda (left right)
                                     (string= (car left) (car right)))))
           (fields-matching (cl-intersection
                             model-fields (mapcar #'car named-fields)
                             :test #'string=))  ; Fields that match the model
	       (fields-missing (cl-set-difference
                            model-fields (mapcar #'car named-fields)
			                :test #'string=))
	       (fields-extra (cl-set-difference
			              (mapcar #'car subheading-fields) model-fields
			              :test #'string=))  ; Fields that are not in the model but could be used by Anki to fill missing fields
	       (fields (cl-loop for field-name in fields-matching
                            for value = (alist-get
					                     field-name named-fields
					                     nil nil #'string=)
			                collect (cons field-name (anki-editor--export-field value format))))
	       (heading-format anki-editor-prepend-heading-format))


      ;; The resources we might have to fill the existing data are
      ;;
      ;; 1. Heading, but only if prepend-heading is nil
      ;;
      ;; 2. Content including extra fields enriched with heading if
      ;; prepend-heading is t

      ;; Consider using `thunk-let' here, so we don't calculate the
      ;; `body-field' if we don't have enough fields anyhow
      (let* ((formatted-heading (or (and format (anki-editor--export-field (format heading-format heading) format))
                                    heading))
             (heading-field (unless prepend-heading formatted-heading))
             (formatted-subheading-fields (mapcar (lambda (field) (anki-editor--export-field (cdr field) format)) subheading-fields))
             (body-field (seq-reduce (lambda (acc value) (string-trim (concat acc "\n\n" (string-trim (or value "")))))
                                     (list
                                      (when prepend-heading formatted-heading)
                                      (anki-editor--export-field content-before-subheading format)
                                      (anki-editor--concat-fields fields-extra formatted-subheading-fields level))
                                     ""))
             ;; `field-pool' ia list of all extra fields available to fill the missing fields
             (field-pool (remq nil (list heading-field body-field))))

        (when (< (length field-pool) (length fields-missing))
          (user-error "Not enough fields to fill the missing fields"))

        ;; For each missing field we take one field from the pool
        (cl-loop for missing-field in fields-missing
                 for field = (pop field-pool)
                 do (push (cons missing-field field) fields)))
      fields)))

(defun anki-editor--concat-fields (field-names field-alist level)
  "Concat field names and content of fields in list `field-names'."

  ;; I think - since format is defined here, we should use it.
  (let ((format (anki-editor-entry-format)))
    (cl-loop for f in field-names
             for value = (alist-get f field-alist nil nil #'string=)
             when value
	         concat (concat (make-string (+ 1 level) ?*) " " f "\n\n"
			                (string-trim value) "\n\n"))))


(defun anki-editor--concat-multivalued-property-value (prop value)
  (let ((old-values (org-entry-get-multivalued-property nil prop)))
    (unless (string-suffix-p prop "+")
      (setq old-values (cl-set-difference old-values
                                          (org-entry-get-multivalued-property
                                           nil (concat prop "+"))
                                          :test 'string=)))
    (mapconcat #'org-entry-protect-space
               (append old-values (list value))
               " ")))

(setq org-properties-postprocess-alist
      (append org-properties-postprocess-alist
              (list (cons anki-editor-prop-tags
                          (lambda (value)
                            (anki-editor--concat-multivalued-property-value
			     anki-editor-prop-tags value)))
                    (cons anki-editor-prop-tags-plus
                          (lambda (value)
                            (anki-editor--concat-multivalued-property-value
			     anki-editor-prop-tags-plus value))))))

;;;###autoload
(define-minor-mode anki-editor-mode
  "A minor mode for making Anki cards with Org."
  :lighter " anki-editor"
  :keymap (make-sparse-keymap)
  (unless (equal major-mode 'org-mode)
    (user-error "anki-editor only works in org-mode buffers"))
  (if anki-editor-mode
      (anki-editor-setup-minor-mode)
    (anki-editor-teardown-minor-mode)))

(defun anki-editor-setup-minor-mode ()
  "Set up this minor mode."
  (anki-editor-api-check)
  (add-hook 'org-property-allowed-value-functions
	    #'anki-editor--get-allowed-values-for-property nil t)
  (advice-add 'org-set-tags :before #'anki-editor--before-set-tags)
  (advice-add 'org-get-buffer-tags :around #'anki-editor--get-buffer-tags)
  (advice-add 'org-html-link :around #'anki-editor--ox-html-link))

(defun anki-editor-teardown-minor-mode ()
  "Tear down this minor mode."
  (remove-hook 'org-property-allowed-value-functions
	       #'anki-editor--get-allowed-values-for-property t))


;;; Commands

(defvar anki-editor--note-markers nil)

(defun anki-editor--collect-note-marker ()
  (message "Scanning notes %d (%s@%d), wait a moment..."
           (length anki-editor--note-markers) (buffer-name) (point))
  (push (point-marker) anki-editor--note-markers))

(defun anki-editor-push-notes (&optional scope match &rest skip)
  "Build notes from headings that MATCH within SCOPE and push them to Anki.

The default search condition `&ANKI_NOTE_TYPE<>\"\"' will always
be appended to MATCH.

For notes that already exist in Anki (i.e. has `ANKI_NOTE_ID'
property), only their fields, tags and deck will be updated,
change of note type is currently not supported.

If SCOPE is not specified, the following rules are applied to
determine the scope:

- If there's an active region, it will be set to `region'
- If called with prefix `C-u', it will be set to `tree'
- If called with prefix double `C-u', it will be set to `file'
- If called with prefix triple `C-u', will be set to `agenda'

See doc string of `org-map-entries' for what these different options mean.

If one fails, the failure reason will be set in property drawer
of that heading."
  (interactive (list (cond
                      ((region-active-p) 'region)
                      ((equal current-prefix-arg '(4)) 'tree)
                      ((equal current-prefix-arg '(16)) 'file)
                      ((equal current-prefix-arg '(64)) 'agenda)
                      (t nil))))
  (unwind-protect
      (progn
        (apply #'anki-editor-map-note-entries
	       #'anki-editor--collect-note-marker match scope skip)
        (setq anki-editor--note-markers (reverse anki-editor--note-markers))
        (let ((count 0)
              (failed 0))
          (save-excursion
            (anki-editor--with-collection-data-updated
              (cl-loop with bar-width = 30
                       for marker in anki-editor--note-markers
                       for progress = (/ (float (cl-incf count))
					 (length anki-editor--note-markers))
                       do
                       (goto-char marker)
                       (message
			"Uploading notes in buffer %s%s [%s%s] %d/%d (%.2f%%)"
                        (marker-buffer marker)
                        (if (zerop failed)
                            ""
                          (propertize (format " %d failed" failed)
                                      'face `(:foreground "red")))
                        (make-string (truncate (* bar-width progress))
				     ?#)
                        (make-string (- bar-width
					(truncate (* bar-width
						     progress)))
				     ?.)
                        count
                        (length anki-editor--note-markers)
                        (* 100 progress))
                       (anki-editor--clear-failure-reason)
                       (condition-case-unless-debug err
                           (anki-editor--push-note (anki-editor-note-at-point))
                         (error (cl-incf failed)
                                (anki-editor--set-failure-reason
				 (error-message-string err))))
                       ;; free marker
                       (set-marker marker nil))))
          (message
           (cond
            ((zerop (length anki-editor--note-markers))
	     "Nothing to push")
            ((zerop failed)
	     (format "Successfully pushed %d notes to Anki" count))
            (t
	     (format (concat "Pushed %d notes to Anki, with %d failed. "
			     "Check property drawers for details. "
			     "\nWhen you have fixed those issues, "
			     "try re-push the failed ones with "
			     "\n`anki-editor-retry-failed-notes'.")
		     count failed))))))
    ;; clean up markers
    (cl-loop for m in anki-editor--note-markers
	     do (set-marker m nil)
	     finally do (setq anki-editor--note-markers nil))))

(defun anki-editor-push-note-at-point ()
  "Push note at point to Anki.

If point is not at a heading with an `ANKI_NOTE_TYPE' property,
go up one heading at a time, until heading level 1, and push the
subtree associated with the first heading that has one."
  (interactive)
  (save-excursion
    (let ((note-type))
      (while
	  (and (org-back-to-heading)
	       (not (setq note-type
			  (org-entry-get nil anki-editor-prop-note-type)))
	       (org-up-heading-safe)))
      (if (not note-type)
	  (user-error "No note to push found")
	(anki-editor--push-note (anki-editor-note-at-point))
	(message "Successfully pushed note at point to Anki.")))))

(defun anki-editor-push-new-notes (&optional scope)
  "Push note entries without ANKI_NOTE_ID in SCOPE to Anki."
  (interactive)
  (anki-editor-push-notes scope (concat anki-editor-prop-note-id "=\"\"")))

(defun anki-editor-retry-failed-notes (&optional scope)
  "Retry pushing notes marked as failed.
This command just calls `anki-editor-push-notes' with match string
matching non-empty `ANKI_FAILURE_REASON' properties."
  (interactive)
  (anki-editor-push-notes scope
			  (concat anki-editor-prop-failure-reason "<>\"\"")))

(defun anki-editor-delete-note-at-point (&optional prefix)
  "Delete the note at point from Anki.
With PREFIX also delete it from Org."
  (interactive "P")
  (save-excursion
    (let (note-type note-id)
      (while
	  (and (org-back-to-heading)
	       (not (setq note-type
			  (org-entry-get nil anki-editor-prop-note-type)))
	       (org-up-heading-safe)))
      (when (not note-type)
	(user-error "No note to delete found"))
      (setq note-id (condition-case nil
			(string-to-number
			 (org-entry-get nil anki-editor-prop-note-id))
		      (error nil)))
      (if (not note-id)
	  (if prefix
	      (message "Note at point is not in Anki (no note-id)")
	    (user-error "Note at point is not in Anki (no note-id)"))
	(when (yes-or-no-p
	       (format (concat "Do you really want to delete note %s "
			       "from Anki?")
		       note-id))
	  (anki-editor-api-call-result 'deleteNotes
				       :notes (list note-id))
	  (org-entry-delete nil anki-editor-prop-note-id)
	  (message "Deleted note %s from Anki" note-id)))
      (when prefix
	(org-mark-subtree)
	(kill-region nil nil t)
	(message "Deleted note at point from Org")))))

(defun anki-editor-insert-note (&optional prefix note-type)
  "Insert a note interactively.

The note is placed after the current subtree, at the same level
as the heading closest before point.

When note heading is not provided, it is used as the first field;
when additionally the note-type only has two fields, the content
after the heading is used for the second field and no subheading
is created.

With `anki-editor-insert-note-always-use-content' the content
after the note heading and before the first subheading is always
used for a field (the second or first field, depending on whether
the heading is used for the first field or not). PREFIX temporarily
inverts the value of `anki-editor-insert-note-always-use-content'.

When NOTE-TYPE is nil, prompt for one."
  (interactive "P")
  (let* ((deck (or (org-entry-get-with-inheritance anki-editor-prop-deck)
                   (completing-read "Deck: " (sort (anki-editor-deck-names)
						   #'string-lessp))))
         (type (or note-type
		   (completing-read "Note type: " (sort
						   (anki-editor-note-types)
						   #'string-lessp))))
         (fields (anki-editor-api-call-result 'modelFieldNames
					      :modelName type))
         (heading (read-from-minibuffer "Note heading (optional): ")))
    (anki-editor--insert-note-skeleton prefix deck heading type fields)))

(defun anki-editor-insert-default-note (&optional prefix)
  "Insert a note with default note type interactively.
The note type is taken from the ANKI_DEFAULT_NOTE_TYPE property,
with inheritance, or from `anki-editor-default-note-type'.
Otherwise this command is like `anki-editor-insert-note'."
  (interactive "P")
  (let ((note-type
	 (or (org-entry-get-with-inheritance
	      anki-editor-prop-default-note-type)
	     anki-editor-default-note-type
	     (user-error "No default note type set"))))
    (anki-editor-insert-note prefix note-type)))

(defun anki-editor-set-note-type (&optional prefix note-type)
  "Set note type for current or closest previous heading.
With PREFIX set note type for all top-level headings in subtree.
When NOTE-TYPE is nil, prompt for one."
  (interactive "P")
  (let ((note-type
	 (or note-type
	     (completing-read "Note type: " (sort
					     (anki-editor-note-types)
					     #'string-lessp))))
	(level
         (if prefix
	     (+ 1 (or (org-current-level) 0))
	   (or (org-current-level) 0))))
    (org-map-entries
     (lambda () (org-set-property anki-editor-prop-note-type note-type))
     (concat "LEVEL=" (number-to-string level))
     (if (and prefix
	      (equal 1 level))
	 nil
       'tree))))

(defun anki-editor-set-default-note-type (&optional prefix)
  "Set default note type for current or closest previous heading.
The note type is taken from the ANKI_DEFAULT_NOTE_TYPE property,
with inheritance, or from `anki-editor-default-note-type'.
Otherwise this command is like `anki-editor-set-note-type'."
  (interactive "P")
  (let ((note-type
	 (or (org-entry-get-with-inheritance
	      anki-editor-prop-default-note-type)
	     anki-editor-default-note-type
	     (user-error "No default note type set"))))
    (anki-editor-set-note-type prefix note-type)))

(defun anki-editor-cloze-region (&optional arg hint)
  "Cloze region with number ARG."
  (interactive "p\nsHint (optional): ")
  (unless (region-active-p) (user-error "No active region"))
  (anki-editor-cloze (region-beginning) (region-end) arg hint))

(defun anki-editor-cloze-dwim (&optional arg hint)
  "Cloze current active region or a word the under the cursor."
  (interactive "p\nsHint (optional): ")
  (cond
   ((region-active-p)
    (anki-editor-cloze (region-beginning) (region-end) arg hint))
   ((thing-at-point 'word)
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (anki-editor-cloze (car bounds) (cdr bounds) arg hint)))
   (t (user-error "Nothing to create cloze from"))))

(defun anki-editor-cloze (begin end arg hint)
  "Cloze region from BEGIN to END with number ARG."
  (let ((region (buffer-substring begin end)))
    (save-excursion
      (delete-region begin end)
      (insert (with-output-to-string
                (princ (format "{{c%d::%s" (or arg 1) region))
                (unless (string-blank-p hint) (princ (format "::%s" hint)))
                (princ "}}"))))))

(defun anki-editor-export-subtree-to-html ()
  "Export subtree of the element at point to HTML."
  (interactive)
  (org-export-to-buffer
      anki-editor--ox-anki-html-backend
      "*AnkiEditor HTML Output*" nil t nil t
      anki-editor--ox-export-ext-plist #'html-mode))

(defun anki-editor-convert-region-to-html ()
  "Convert and replace region to HTML."
  (interactive)
  (org-export-replace-region-by anki-editor--ox-anki-html-backend))


;;; More utilities

(defun anki-editor-api-check ()
  "Check if correct version of AnkiConnect is serving."
  (interactive)
  (let ((ver (condition-case err
                 (anki-editor-api-call-result 'version)
               (error (error (concat "Failed to connect to Anki: %s"
				     "\nIs Anki running with the "
				     "AnkiConnect add-on enabled?")
			     (error-message-string err))))))
    (if (<= anki-editor-api-version ver)
        (when (called-interactively-p 'interactive)
          (message "AnkiConnect v.%d is running" ver))
      (user-error "anki-editor requires at least version %d of AnkiConnect"
		  anki-editor-api-version))))

(defun anki-editor-sync-collection ()
  "Synchronize the local Anki collection with AnkiWeb."
  (interactive)
  (anki-editor-api-call-result 'sync)
  (message "Synced local Anki collection with AnkiWeb."))

(defun anki-editor-gui-browse (&optional query)
  "Open Anki Browser with QUERY.
When called interactively, it will try to set QUERY to current
note or deck."
  (interactive
   (list
    (pcase (org-entry-get-with-inheritance anki-editor-prop-note-id)
      ((and (pred stringp) nid) (format "nid:%s" nid))
      (_ (format "deck:%s"
                 (or (org-entry-get-with-inheritance anki-editor-prop-deck)
                     "current"))))))
  (anki-editor-api-call 'guiBrowse :query (or query ""))
  (when anki-editor-gui-browse-ensure-foreground
    (anki-editor-api-call 'guiBrowse :query (or query ""))))

(defun anki-editor-gui-add-cards ()
  "Open Anki Add Cards dialog with presets from current note entry."
  (interactive)
  (anki-editor-api-call-result 'guiAddCards
                               :note (append
                                      (anki-editor-api--note
                                       (anki-editor-note-at-point))
                                      (list :options '(:closeAfterAdding t)))))

(defun anki-editor-find-notes (&optional query)
  "Find notes with QUERY."
  (interactive "sQuery: ")
  (let ((nids (anki-editor-api-call-result 'findNotes
                                           :query (or query ""))))
    (if (called-interactively-p 'interactive)
        (message "%S" nids)
      nids)))

(defvar anki-editor--style-start "</style>\n<!-- {{ Emacs Org-mode -->")
(defvar anki-editor--style-end "<!-- Emacs Org-mode }} -->\n<style>")

(defun anki-editor-copy-styles ()
  "Copy `org-html-style-default' and `anki-editor-html-head' to Anki."
  (interactive)
  (let ((head (concat (org-element-normalize-string anki-editor--style-start)
                      (org-element-normalize-string
		       (format "<!-- Updated: %s -->" (current-time-string)))
                      (when anki-editor-include-default-style
                        (org-element-normalize-string org-html-style-default))
                      (org-element-normalize-string anki-editor-html-head)
                      anki-editor--style-end)))
    (cl-loop for model in (anki-editor-note-types)
             for style = (let* ((css (alist-get
				      'css
				      (anki-editor-api-call-result
				       'modelStyling :modelName model)))
                                (start (string-match
                                        (regexp-quote anki-editor--style-start)
                                        css))
                                (end (string-match
                                      (regexp-quote anki-editor--style-end)
                                      css)))
                           (if (and start end)
                               (progn
                                 (cl-incf end (length anki-editor--style-end))
                                 ;; skip whitespaces
                                 (when-let ((newend (string-match
						     "[[:graph:]]" css end)))
                                   (setq end newend))
                                 (concat
                                  (substring css 0 start)
                                  (substring css end)))
                             css))
             do
             (message "Updating styles for \"%s\"..." model)
             (anki-editor-api-call-result
	      'updateModelStyling
              :model (list :name model
                           :css (concat (concat head "\n\n") style)))
             finally do (message "Updating styles...Done"))))

(defun anki-editor-remove-styles ()
  "Remove html tags generated by this mode from card styles."
  (interactive)
  (cl-loop for model in (anki-editor-note-types)
           for css = (alist-get 'css (anki-editor-api-call-result
				      'modelStyling :modelName model))
           for start = (string-match
                        (regexp-quote anki-editor--style-start)
                        css)
           for end = (string-match
                      (regexp-quote anki-editor--style-end)
                      css)
           if (and start end)
           do
           (cl-incf end (length anki-editor--style-end))
           ;; also remove whitespaces
           (when-let ((newend (string-match "[[:graph:]]" css end)))
             (setq end newend))
           (message "Resetting styles for \"%s\"..." model)
           (anki-editor-api-call-result
            'updateModelStyling
            :model (list :name model
                         :css (concat
                               (substring css 0 start)
                               (substring css end))))
           finally do (message "Resetting styles...Done")))


(provide 'anki-editor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anki-editor.el ends here
