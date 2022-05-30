;;; ref-man-export.el --- Document export and publishing functionality for `ref-man'. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019,2020,2021,2022
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Monday 23 May 2022 09:24:34 AM IST>
;; Keywords:	pdfs, references, bibtex, org, eww

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Export functions for `ref-man'.
;;
;; We use `pandoc' for exporting.  Currently we export from an `org-mode'
;; buffers but other modes can also be handled easily.
;;
;; We use markdown as intermediate format for exporting as then it can be used
;; by many other frameworks and `pandoc' has good support for embedding extra
;; metadata in markdown.  While data like username and email can be setup easily
;; in `org', we also generate the bibliography directly from org text
;; links.  Which means we either keep temporary .bib files corresponding to each
;; generated file or embed the bibliography in the metadata.  Putting all that
;; data inside the org properties or any other drawer would be redundant, so we
;; include that information in the yaml header in the markdown file.

;;; Code:

(require 'util)
(require 'ox)
(require 'ox-gfm)
(require 'yaml)                         ; TODO: yaml is actually not used
(require 'ref-man-core)

(defcustom ref-man-export-blog-dir (expand-file-name "~/.ref-man/blog/")
  "Directory where the org files corresponding to documents will be stored."
  :type 'directory
  :group 'ref-man)

(defcustom ref-man-export-pandoc-templates-dir (expand-file-name "~/.pandoc/templates")
  "Directory for pandoc templates."
  :type 'directory
  :group 'ref-man)

(defcustom ref-man-export-pandoc-csl-dir (expand-file-name "~/.pandoc/csl")
  "Directory for csl files."
  :type 'directory
  :group 'ref-man)

(defcustom ref-man-export-author-name ""
  "Name of the author for export articles."
  :type 'string
  :group 'ref-man)

(defcustom ref-man-export-author-email ""
  "Default email of the author for export articles."
  :type 'string
  :group 'ref-man)

(defcustom ref-man-export-author-affiliation ""
  "Default affiliation key of the author for export articles.

The affiliation details are the values of the alist.
See `ref-man-research-article-affiliations-alist'."
  :type 'string
  :group 'ref-man)

(defcustom ref-man-research-article-affiliations-alist nil
  "Affiliation alist of the author for export articles."
  :type 'alist
  :group 'ref-man)

;; TODO: Import from an alist like `ref-man-export-research-article-args'
(defcustom ref-man-export-default-opts
  '(("title" . "")
    ("author" . "")
    ("link-citations" . t)
    ("mathjax" . "")
    ("csl" . ""))
  "Default options for `ref-man-export'."
  :type 'alist
  :group 'ref-man)

(defcustom ref-man-export-blog-extra-opts
  '(("date" . "")
    ("category" . "")
    ("tags" . "")
    ("keywords" . "")
    ("draft" . t))

  "Default blog options for `ref-man-export'."
  :type 'alist
  :group 'ref-man)

(defcustom ref-man-export-research-article-args nil
  "Extra arguments for exporting a research article.
Should be an `alist' parseable by `yaml-encode'."
  :type 'alist
  :group 'ref-man)

(defcustom ref-man-export-journal-args nil
  "Extra journal specific arguments."
  :type 'alist
  :group 'ref-man)

(defcustom ref-man-export-output-dir ""
  "Export output directory."
  :type 'directory
  :group 'ref-man)

(defcustom ref-man-export-mathjax-dir ""
  "Directory of docproc."
  :type 'directory
  :group 'ref-man)

(defcustom ref-man-export-bib-no-warn-types '((blog . ("http" "https")))
  "Don't warn for these links."
  :type 'alist
  :group 'ref-man)

(defcustom ref-man-export-csl-urls-file nil
  "Path to csl file which would allow including urls."
  :type 'file
  :group 'ref-man)

(defcustom ref-man-export-csl-no-urls-file nil
  "Path to csl file which would not allow including urls."
  :type 'file
  :group 'ref-man)

(defcustom ref-man-export-python-executable "/usr/bin/python3"
  "Path to the python executable for calling `pndconf'."
  :type 'file
  :group 'ref-man)

(defvar ref-man-export-temp-org-buf " *ref-man-export-org-buf*"
  "Intermediate org buffer for preprocessing while exporting.")

(defvar ref-man-export-temp-md-buf " *ref-man-export-md-buf*"
  "Markdown buffer name where org buffer is exported.")

(defvar ref-man-export-pre-export-md-functions
  '(ref-man-replace-multiple-spaces-with-a-single-space)
  "Functions to run on org buffer just before export to markdown.

Functions added to this hook should run with no arguments on the
current buffer.  The functions would manipulate the org buffer
accordingly and must preserve the org buffer structure.

The default value is
`ref-man-replace-multiple-spaces-with-a-single-space' which is a
clean up function replacing multiple spaces with a single space.")

(defvar ref-man-export-metadata-hook '(ref-man-export-get-paper-metadata
                                       ref-man-export-get-article-metadata)
  "Hook to run while exporting org properties as pandoc metadata.

The functions in this hook should modify
`ref-man-export-metadata' by appending to it an alist of any
custom metadata extraction method.

See `ref-man-export-get-paper-metadata' and
`ref-man-export-get-journal-metadata' for examples.")

(defvar ref-man-export-journal-specific-metadata-hook nil
  "Like `ref-man-export-metadata-hook' but for individual journals/venues.")

(defvar ref-man-export-metadata nil
  "Variable to gather results of `ref-man-export-metadata-hook'.")

(defcustom ref-man-export-pdflatex-env-vars ""
  "Environment variables for pdflatex."
  :type 'string
  :group 'ref-man)

(defcustom ref-man-export-paper-version-org-file nil
  "Save current org version file to disk for output type 'paper.

When non-nil write an org file with md5 sum suffix also along
with markdown file.  Only for output type 'paper."
  :type 'boolean
  :group 'ref-man)

(defcustom ref-man-export-no-confirm-overwrite nil
  "Do not confirm to overwrite markdown file.
This variable controls the global confirmation behaviour, while
for individual documents it can be set as NO_CONFIRM nil or t
in the properties drawer of the subtree."
  :type 'boolean
  :group 'ref-man)

(defvar ref-man-export-pndconf-config-file nil
  "`pndconf' config file.")

;; TEST
;; (with-current-buffer "test.org"
;;   (let ((ref-man-export-no-confirm-overwrite t))
;;     (pcase (org-entry-get (point) "NO_CONFIRM" nil t)
;;       ("t" t)
;;       ("nil" nil)
;;       (_ ref-man-export-no-confirm-overwrite))))

(defun ref-man-export-templates ()
  "Get templates as an alist from `ref-man-export-pandoc-templates-dir'."
  (mapcar
   (lambda (x) (cons
                (downcase (replace-regexp-in-string "\\.template\\|default\\." ""
                                                    (f-filename x)))
                x))
   (f-files ref-man-export-pandoc-templates-dir)))

(defun ref-man-export-csl-files (csl-file)
  "Get the full path for CSL-FILE.

Files are searched in `ref-man-export-pandoc-csl-dir'"
  (a-get (mapcar
          (lambda (x) (cons
                       (downcase (string-remove-suffix ".csl" (f-filename x)))
                       x))
          (-filter (lambda (x) (string-suffix-p ".csl" (f-filename x)))
                   (f-files ref-man-export-pandoc-csl-dir)))
         (string-remove-suffix ".csl" csl-file)))

;; TODO: This should be a macro
(defun ref-man-export-pdf-template ()
  "Get template for PDF generation."
  (a-get (ref-man-export-templates)
         (or (org-entry-get (point) "TEMPLATE") "latex")))

(defun ref-man-export-paper-template ()
  "Get template for paper generation."
  (a-get (ref-man-export-templates)
         (or (org-entry-get (point) "TEMPLATE") "ieee")))

(defun ref-man-export-html-template ()
  "Get template for html generation."
  (a-get (ref-man-export-templates)
         (or (org-entry-get (point) "TEMPLATE") "blog")))

;; NOTE: For customization of filtering various org elements
(org-export-define-derived-backend 'ref-md 'html
  :filters-alist '((:filter-parse-tree . org-md-separate-elements))
  :menu-entry
  '(?m "Export to Markdown"
       ((?M "To temporary buffer"
	    (lambda (a s v b) (org-md-export-as-markdown a s v)))
	(?m "To file" (lambda (a s v b) (org-md-export-to-markdown a s v)))
	(?o "To file and open"
	    (lambda (a s v b)
	      (if a (org-md-export-to-markdown t s v)
		(org-open-file (org-md-export-to-markdown nil s v)))))))
  :translate-alist '((bold . org-md-bold)
		     (center-block . org-md--convert-to-html)
		     (code . org-md-verbatim)
		     (drawer . org-md--identity)
		     (dynamic-block . org-md--identity)
		     (example-block . org-md-example-block)
		     (export-block . org-md-export-block)
		     (fixed-width . org-md-example-block)
		     (headline . org-md-headline)
		     (horizontal-rule . org-md-horizontal-rule)
		     (inline-src-block . org-md-verbatim)
		     (inlinetask . org-md--convert-to-html)
		     (inner-template . org-md-inner-template)
		     (italic . org-md-italic)
		     (item . org-md-item)
		     (keyword . org-md-keyword)
		     (line-break . org-md-line-break)
		     (link . org-md-link)
		     (node-property . org-md-node-property)
		     (paragraph . org-md-paragraph)
		     (plain-list . org-md-plain-list)
		     (plain-text . org-md-plain-text)
		     (property-drawer . org-md-property-drawer)
		     (quote-block . org-md-quote-block)
		     (section . org-md-section)
		     (special-block . org-md--convert-to-html)
		     (src-block . org-md-example-block)
		     (table . org-md-verbatim)
		     (template . org-md-template)
		     (verbatim . org-md-verbatim))
  :options-alist
  '((:md-footnote-format nil nil org-md-footnote-format)
    (:md-footnotes-section nil nil org-md-footnotes-section)
    (:md-headline-style nil nil org-md-headline-style)))


;; NOTE: Copied from `ox-gfm' in case modifications are needed
(org-export-define-derived-backend 'ref-gfm 'gfm
  :filters-alist '((:filter-parse-tree . org-md-separate-elements))
  :menu-entry
  '(?g "Export to Github Flavored Markdown"
       ((?G "To temporary buffer"
            (lambda (a s v b) (org-gfm-export-as-markdown a s v)))
        (?g "To file" (lambda (a s v b) (org-gfm-export-to-markdown a s v)))
        (?o "To file and open"
            (lambda (a s v b)
              (if a (org-gfm-export-to-markdown t s v)
                (org-open-file (org-gfm-export-to-markdown nil s v)))))))
  :translate-alist '((inner-template . org-gfm-inner-template)
                     (paragraph . org-gfm-paragraph)
                     (strike-through . org-gfm-strike-through)
                     (src-block . org-gfm-src-block)
                     (table-cell . org-gfm-table-cell)
                     (table-row . org-gfm-table-row)
                     (table . org-gfm-table)))


(defun ref-man-export-org-to-md (type &optional subtree output-org-file)
  "Copy the org buffer to temp buffer and export to markdown.

We convert all internal org links to file-local links so that
they'll be subbed with citation formats later.

Hook `ref-man-export-pre-export-md-functions' is run before export to markdown.

TYPE controls the backend used to generate the markdown buffer.

With optional non-nil SUBTREE, export only subtree.

If optional OUTPUT-ORG-FILE is non-nil, then the buffer is
written to it."
  (save-restriction
    (let ((buf-string (if (not subtree)
                          (buffer-string)
                        (org-narrow-to-subtree)
                        (buffer-string)))
          (link-re ref-man-file-fuzzy-custid-link-re)
          (backend (pcase type
                     ((or 'html 'pdf 'blog 'both) 'ref-gfm)
                     ('paper 'ref-md)
                     (_ nil)))
          (ref-man-export-pre-export-md-functions
           '(util/org-remove-all-time-stamps ref-man-replace-multiple-spaces-with-a-single-space))
          org-export-show-temporary-export-buffer)
      (with-current-buffer (get-buffer-create ref-man-export-temp-org-buf)
        (erase-buffer)
        (insert buf-string)
        (org-mode)
        ;; NOTE: Promote subtree to top level
        (while (progn (goto-char (point-min))
                      (and (or (org-at-heading-p)
                               (outline-next-heading))
                           (> (org-current-level) 1)))
          (while (re-search-forward "\\(^\\*+\\)\\( +\\)\\(.+\\)" nil t)
            (beginning-of-line)
            (delete-char 1)
            (end-of-line)))
        (when output-org-file
          (write-region (point-min) (point-max) output-org-file))
        (goto-char (point-min))
        (while (re-search-forward link-re nil t)
          (pcase-let ((`(,a ,b) (list (match-string 1) (match-string 2))))
            (replace-match
             (format "[[%s][%s]]" (replace-regexp-in-string "file:.+::" "" a) b))))
        (run-hook-with-args 'ref-man-export-pre-export-md-functions)
        (org-export-to-buffer backend ref-man-export-temp-md-buf nil nil nil nil)))))


(defun ref-man-export-get-abstract-bibtexs (type metadata)
  "Get abstract and bibtexs from the buffer.

Org buffer may be possibly narrowed.

TYPE is the type of article being processed.
METADATA is the existing metadata.

METADATA is modified and returned in a list along with bibtexs and abstract."
  (let (abstract bibtexs)
    ;; NOTE: Get bounds of article
    (pcase type
      ('paper (let ((beg (a-get metadata "sections-beg"))
                    (end (a-get metadata "sections-end")))
                ;; NOTE: When it's a paper, first paragraph is abstract and we
                ;;       go to first subheading
                (setq abstract (a-get metadata "abstract"))
                (narrow-to-region beg end)
                (setq metadata (a-dissoc metadata "abstract" "doc-root" "sections-beg" "sections-end"))))
      (_ nil))
    (setq bibtexs (ref-man-export-parse-references type (a-get ref-man-export-bib-no-warn-types type)))
    (setq bibtexs (mapcar (lambda (x)
                            `(,(car x) ,(nth 1 x) ,(replace-regexp-in-string "venue=" "booktitle=" (nth 2 x))))
                          bibtexs))
    (list abstract bibtexs metadata)))


(defun ref-man-export-get-opts (type title bib-file mathjax-path csl-file
                                            &optional no-refs)
  "Generate alist for yaml metadata for `ref-man-export-docproc-article'.

TYPE is one of 'html 'pdf 'both 'blog, TITLE is the title of the
article, BIB-FILE is the additional bibliography file for
citations, MATHJAX-PATH is the path where Mathjax scripts would
be located and CSL-FILE is the Citation Style File.

Don't include bibliography when NO-REFS is non-nil."
  (let ((opts ref-man-export-default-opts)
        (blog-opts ref-man-export-blog-extra-opts)
        (author ref-man-export-author-name))
    (setq opts (a-assoc opts
                        "title" title
                        "author" author
                        "mathjax" mathjax-path
                        "csl" csl-file))
    (unless no-refs
      (setq opts (a-assoc opts
                          "bibliography"
                          (if bib-file
                              (cons bib-file ref-man-bib-files)
                            ref-man-bib-files))))
    (when (eq type 'blog)
      (setq opts
            (a-assoc opts
                     "date" (time-stamp-string "%Y-%m-%d")
                     "category" (or (util/org-get-tree-prop "CATEGORY_ROOT" t)
                                    (downcase (let ((category (ref-man-org-ask-to-set-property "BLOG_CATEGORY")))
                                                (if (and category (not (string-empty-p category)))
                                                    category
                                                  (user-error "Cannot export to blog without category")))))

                     "tags" (or (org-get-tags nil t)
                                (downcase (let ((blog-tags (ref-man-org-ask-to-set-property "BLOG_TAGS")))
                                            (if (and blog-tags (not (string-empty-p blog-tags)))
                                                blog-tags
                                              (user-error "Cannot export to blog without tags")))))
                     "keywords" (downcase (or (ref-man-org-ask-to-set-property "BLOG_KEYWORDS")
                                              (ref-man-org-ask-to-set-property "BLOG_TAGS"))))))
    (when (eq type 'article)
      (setq opts
            (a-assoc opts
                     "date" (time-stamp-string "%Y-%m-%d")
                     "toc" t
                     "toc-depth" 2)))
    opts))

(defun ref-man-export-bibtexs (bibtexs citeproc &optional tmp-bib-file no-gdrive)
  "Export the references from BIBTEXS.
BIBTEXS is a string of bibliography entries.

CITEPROC is the citation processor to use.  Can be one of
`bibtex' or `biblatex'.

If optional TMP-BIB-FILE is given then write to that file.
Default is to export the BIBTEXS string as yaml metadata which is
read by `pandoc'.  Optional NO-GDRIVE implies to remove the
gdrive keys if present."
  (when bibtexs
    (with-temp-buffer
      (insert (mapconcat (lambda (x)
                           (ref-man--replace-non-ascii (nth 2 x)))
                         bibtexs ""))
      (unless no-gdrive
        (goto-char (point-min))
        ;; Replace "gdrive" with "issn" in bibs
        (while (re-search-forward "gdrive=" nil t nil)
          (replace-match "issn=")))
      (cond (tmp-bib-file
             (write-region (point-min) (point-max) tmp-bib-file))
            ;; NOTE: Write to a temp file if \' accents are present
            ((string-match-p "'" (buffer-string))
             (let ((temp-file (make-temp-file ".tmp-bib"))
                   (cur (point-max)))
               (write-region (point-min) (point-max) temp-file)
               (goto-char cur)
               (insert (replace-regexp-in-string
                        "^---\\|^nocite.*" ""
                        (shell-command-to-string (format "%s -s -f %s -t markdown %s"
                                                         ref-man-pandoc-executable
                                                         citeproc
                                                         temp-file))))
               (delete-file temp-file)
               (util/delete-blank-lines-in-region cur (point-max))
               (buffer-substring-no-properties cur (point-max))))
            (t
             (let ((cur (point-max)))
               (goto-char cur)
               (insert (replace-regexp-in-string
                        "^---\\|^nocite.*" ""
                        (shell-command-to-string (format "%s -s -f %s -t markdown <<<'%s'"
                                                         ref-man-pandoc-executable
                                                         citeproc
                                                         (buffer-string)))))
               (util/delete-blank-lines-in-region cur (point-max))
               (buffer-substring-no-properties cur (point-max))))))))

(defun ref-man-export-check-author (author)
  "Check and return AUTHOR type as an alist.

Author is a string of comma separated author names with each
author being combination affiliation:name:email.

If no author or an incorrect type is given then return the
default author alist."
  (if (and author (not (string-empty-p author)))
      (let ((splits (-remove #'string-empty-p (split-string author ","))))
        (when (-all? (lambda (y) (= y 3))
                     (mapcar (lambda (x) (length (split-string x ":"))) splits))
          (mapcar (lambda (x)
                    (unless (string-empty-p x)
                      (pcase-let
                          ((`(,aff ,name ,email)
                            (mapcar 'string-trim (split-string
                                                  (replace-regexp-in-string "<\\|>" "" x) ":"))))
                        `(("affiliation" . ,aff) ("name" . ,name) ("email" . ,email)))))
                  splits)))
    `(("name" . ,ref-man-export-author-name)
      ("email" . ,ref-man-export-author-email)
      ("affiliation" . ,ref-man-export-author-affiliation))))


(defun ref-man-export-get-article-metadata (type)
  "Extract keywords and standard metadata for journal.
TYPE has to be 'paper for this hook to run."
  (when (eq type 'paper)
    (let* ((props (org-entry-properties))
           (author (ref-man-export-check-author (a-get props "AUTHOR")))
           (affiliations (mapcar (lambda (y)
                                   (a-get ref-man-research-article-affiliations-alist y))
                                 (-uniq (mapcar
                                         (lambda (x) (a-get x "affiliation"))
                                         author)))))
      (setq ref-man-export-metadata
            (-concat ref-man-export-metadata
                     `(("author" . ,author)
                       ("keywords" . ,(split-string (a-get props "KEYWORDS") ","))
                       ("caption" . ,(a-get props "CAPTION"))
                       ("affiliations" . ,affiliations)))))))

(defun ref-man-export-get-journal-metadata (type)
  "Extract journal type and template from property drawer.
TYPE has to be 'paper for this hook to run."
  (when (eq type 'paper)
    (let* ((props (org-entry-properties)))
      (setq ref-man-export-metadata
            (-concat ref-man-export-metadata
                     `(("journal" . ,(a-get props "JOURNAL"))
                       ("template" . ,(a-get props "TEMPLATE")))
                     (a-get ref-man-export-journal-args (a-get props "JOURNAL")))))))

(defun ref-man-export-get-paper-metadata (type)
  "Extract abstract and section demarcations for TYPE 'paper."
  (when (eq type 'paper)
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (pcase-let* ((`(,root ,before-refs) (ref-man-org-get-bounds-before-references))
                     (`(,beg ,end ,has-body) (progn (goto-char root)
                                                    (ref-man-org-text-bounds))))
          (when has-body
            (setq ref-man-export-metadata
                  (a-assoc ref-man-export-metadata
                           "abstract" (buffer-substring-no-properties beg end))))
          (setq ref-man-export-metadata
                (a-assoc ref-man-export-metadata
                         "doc-root" root
                         "sections-beg" end
                         "sections-end" before-refs)))))))

(defun ref-man-export-get-all-imgs (&optional buffer)
  "Return list of all file paths in `includegraphics' directives for org subtree.
Optional BUFFER specifies the org buffer."
  (let ((buffer (or buffer (current-buffer)))
        (doc-root (util/org-get-tree-prop "DOC_ROOT"))
        imgs)
    (with-current-buffer buffer
      (unless doc-root
        (user-error "For extraction of images for a research article, a DOC_ROOT must be specified"))
      (save-excursion
        (goto-char doc-root)
        (save-restriction
          (org-narrow-to-subtree)
          (while (re-search-forward "\\includegraphics\\(?:\\[.+?]\\){\\(.+?\\)}" nil t)
            (push (substring-no-properties (match-string 1)) imgs))))
      imgs)))

(defun ref-man-export-copy-standalone-files (&optional tex-file)
  "Copy all supporting files corresponding to a TEX-FILE.

Create a new folder based on the title of the document and copy
the img, supporting tex and bib files for easy upload to
servers."
  (interactive)
  (let* ((tex-file (or tex-file
                       (ido-read-file-name "Enter the tex file path: ")))
         (bib-file (concat (string-remove-suffix ".tex" tex-file) ".bib"))
         (docs-dir ref-man-export-output-dir)
         (buf (find-file-noselect tex-file))
         (doc-name (with-current-buffer buf
                     (progn
                       (goto-char (point-min))
                       (re-search-forward "\\title{\\(\\(.\\|\n\\)?+?\\)}")
                       (downcase
                        (string-join
                         (mapcar #'ref-man--remove-punc
                                 (split-string
                                  (substring-no-properties (match-string 1))))
                         "-")))))
         (old-dir (f-parent tex-file))
         (out-dir (f-join docs-dir (concat doc-name "-standalone")))
         (out-file (f-join out-dir (f-filename tex-file)))
         (out-bib-file (concat (string-remove-suffix ".tex" out-file) ".bib")))
    (kill-buffer buf)
    (when (f-exists? out-dir)
      (f-delete out-dir t))
    (f-mkdir out-dir)
    (copy-file tex-file out-file)
    (copy-file bib-file out-bib-file)
    (let ((buf (find-file-noselect out-file))
          cls-file
          sty-files
          img-files)
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "\\documentclass\\[.*?]{\\(.+?\\)}")
        (setq cls-file (string-trim (shell-command-to-string
                                     (format "kpsewhich %s.cls"
                                             (substring-no-properties (match-string 1))))))
        (goto-char (point-min))
        (while (re-search-forward "\\(\\usepackage\\(?:\\[.+?]\\)?\\){\\(.+?\\)}" nil t)
          (push (split-string (substring-no-properties (match-string 2)) ",") sty-files))
        (goto-char (point-min))
        (while (re-search-forward "\\(\\includegraphics\\(?:\\[.+?]\\)?\\){\\(\\(.\\|\n\\)?+?\\)}" nil t)
          (unless (syntax-ppss-context (syntax-ppss))
            (push (match-string 2) img-files)
            (replace-match (format "%s{%s}"
                                   (match-string 1)
                                   (concat "./" (f-filename (match-string 2)))))))
        (seq-do (lambda (x)
                  (copy-file (string-trim
                              (shell-command-to-string (format "kpsewhich %s.sty" x)))
                             (f-join out-dir (f-filename (concat x ".sty"))) t))
                (-flatten sty-files))
        (seq-do (lambda (x)
                  (copy-file x
                             (f-join out-dir (f-filename x)) t))
                img-files)
        (save-buffer))
      (kill-buffer buf))))


(defun ref-man-export-generate-standalone ()
  "Create a folder with all files required for a self contained document.

Copy the img, tex and bib files in the folder for easy upload to
servers.

Requires the bib and other files to be generated once with
`ref-man-export-docproc-article'."
  ;; NOTE: Perhaps output to a different directory also?
  (interactive)
  (let* ((imgs (ref-man-export-get-all-imgs))
         (doc-root (util/org-get-tree-prop "DOC_ROOT"))
         (docs-dir ref-man-export-output-dir)
         (title (if doc-root
                    (save-excursion
                      (goto-char doc-root)
                      (substring-no-properties (org-get-heading t t t t)))
                  (user-error "For generation of a research article, a DOC_ROOT must be specified")))
         (title-words (split-string (downcase (ref-man--remove-punc title t))))
         (checksum (save-restriction
                     (org-narrow-to-subtree)
                     (md5 (buffer-substring-no-properties (point-min) (point-max)))))
         (doc-name (concat (string-join
                            (-take 3 title-words) "-")
                           "-" (substring checksum 0 10)))
         (existing-files-dir (f-join docs-dir (string-join title-words "-")))
         (out-dir  (concat existing-files-dir "-standalone/"))
         (files (mapcar (lambda (x) (path-join existing-files-dir (concat doc-name x)))
                        '(".tex" ".bib")))
         (compile-cmd (s-lex-format "pdflatex ${doc-name}.tex && bibtex ${doc-name}.aux && pdflatex ${doc-name}.tex && pdflatex ${doc-name}.tex && rm *.out *.aux *.log *.blg"))
         article-file)
    (unless (-all? (lambda (x) (and (f-exists? x) (f-file? x))) files)
      (user-error "Some files for document are missing.  Generate first?"))
    (when (f-exists? out-dir)
      (f-delete out-dir t))
    (f-mkdir out-dir)
    (seq-do (lambda (x) (copy-file x out-dir t)) files)
    (seq-do (lambda (x) (copy-file x out-dir t)) imgs)
    (let ((buf (find-file-noselect (path-join out-dir (concat doc-name ".tex")))))
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "\\documentclass\\[.*?]{\\(.+?\\)}")
        (setq article-file (string-trim (shell-command-to-string
                                         (format "kpsewhich %s.cls"
                                                 (substring-no-properties (match-string 1))))))
        (copy-file article-file out-dir t)
        (goto-char (point-min))
        (while (re-search-forward "\\(\\includegraphics\\(?:\\[.+?]\\)?\\){\\(.+?\\)}" nil t)
          (replace-match (format "\\1{%s}" (concat "./" (f-filename (match-string 2))))))
        (seq-do (lambda (x)
                  (goto-char (point-min))
                  (when (re-search-forward (format "\\(\\usepackage\\(?:\\[.+?]\\)?\\){%s}" (car x)) nil t)
                    (replace-match (format "\\1{%s}" (format "%s" (car x)))) ; (format "sty/%s" (car x))
                    (copy-file (cdr x) out-dir)))
                '(("authblk" .  "/home/joe/texmf/tex/latex/authblk.sty")
                  ("algorithm2e" . "/usr/share/texlive/texmf-dist/tex/latex/algorithm2e/algorithm2e.sty")
                  ("algorithmic" . "/usr/share/texlive/texmf-dist/tex/latex/algorithms/algorithmic.sty")
                  ("algorithmicx" . "/usr/share/texlive/texmf-dist/tex/latex/algorithmicx/algorithmicx.sty")
                  ("algpseudocode" . "/usr/share/texlive/texmf-dist/tex/latex/algorithmicx/algpseudocode.sty")))
        (save-buffer))
      (kill-buffer buf))))

(defun ref-man-export-blog-no-urls (&optional buffer)
  "Export BUFFER as 'blog.

BUFFER defaults to `current-buffer'.

See `ref-man-export-docproc-article' for details."
  (interactive)
  (ref-man-export-docproc-article buffer 'blog t (not current-prefix-arg)
                                  '(:with-toc t :with-tables t)))

(defun ref-man-export-both-no-urls (&optional buffer)
  "Export BUFFER as both 'blog and 'pdf.

BUFFER defaults to `current-buffer'.

See `ref-man-export-docproc-article' for details."
  (interactive)
  (ref-man-export-docproc-article buffer 'both t (not current-prefix-arg)
                                  '(:with-toc t :with-tables t)))

(defun ref-man-export-html-no-urls (&optional buffer)
  "Export BUFFER as 'html.

BUFFER defaults to `current-buffer'.

See `ref-man-export-docproc-article' for details."
  (interactive)
  (ref-man-export-docproc-article buffer 'html t (not current-prefix-arg)
                                  '(:with-toc t :with-tables t)))

(defun ref-man-export-article-no-urls (&optional buffer type)
  "Export BUFFER as a pdf article.

BUFFER defaults to `current-buffer'.
TYPE is passed on to `ref-man-export-docproc-article'.

See `ref-man-export-docproc-article' for details."
  (interactive)
  (ref-man-export-docproc-article buffer (or type (and current-prefix-arg 'html) 'pdf) t t
                                  '(:with-tables t)))

(defun ref-man-export-paper-no-urls (&optional pref-arg buffer)
  "Export BUFFER as a research article.

BUFFER defaults to `current-buffer'.  Optional PLAIN specifies to
export as a plain research article without any journal/conference
templates.

See `ref-man-export-docproc-article' for details."
  (interactive "p")
  (let ((doc-root (util/org-get-tree-prop "DOC_ROOT"))
        (no-cite (> pref-arg 1))
        (force (> pref-arg 4)))
    (unless doc-root
      (user-error "For generation of a research article, a DOC_ROOT must be specified"))
    (save-excursion
      (goto-char doc-root)
      (message "Exporting to PDF...")
      (ref-man-export-docproc-article buffer 'paper t t '(:with-src (ref-man-export-remove-src-blocks))
                                      no-cite force))))

(defun ref-man-export-paper-plain-no-urls (&optional buffer)
  "Export BUFFER as a plain research article.

See `ref-man-export-paper-no-urls'."
  (interactive)
  (ref-man-export-docproc-article buffer 'paper t t '(:with-src (ref-man-export-remove-src-blocks))
                                  nil nil t))

(defcustom ref-man-export-latex-table-template "\\begin{table}
\\centering
%s
\\caption{CAPTION}
\\label{tab:}
\\end{table}
"
  "Template for inserting table with `ref-man-export-table-to-latex'.
The table is inserted at %s and formatted with `format'."
  :type 'string
  :group 'ref-man)

(defcustom ref-man-export-latex-table-no-caption-template "\\begin{table}
\\centering
%s
\\end{table}
"
  "Template for inserting table with `ref-man-export-table-to-latex'.

This one doesn't require the caption package.  The table is
inserted at %s and formatted with `format'."
  :type 'string
  :group 'ref-man)

(defvar ref-man-export-latex-table-minipage-template
  "\\begin{minipage}{\\linewidth}
\\captionof{table}{CAPTION} \\label{}
%s
\\end{minipage}
"
  "Template for to export an table via LaTeX with minipage.")

(defvar ref-man-export-latex-table-document-template
  "\\documentclass{article}
\\usepackage{bbm,amssymb,amsmath,amsfonts}
\\usepackage{algorithm,algpseudocode}
\\begin{document}
%s
\\end{document}"
  "Template for exporting an org table via LaTeX to a standalone document.

Includes basic maths packages.")

(defun ref-man-export-table-to-latex (&optional no-caption)
  "Export an org or table mode table in region to latex."
  (interactive)
  (if (org-at-table-p)
      (let ((org-export-with-broken-links 'mark)
            org-export-show-temporary-export-buffer
            table)
        (save-excursion
          (save-restriction
            (narrow-to-region (org-table-begin) (org-table-end))
            (org-export-to-buffer 'latex "*Latex Export*" nil nil nil nil)
            (setq table (with-current-buffer "*Latex Export*"
                          (goto-char (point-min))
                          (re-search-forward (rx "\\begin{tabular}"))
                          (beginning-of-line)
                          (prog1 (format (if no-caption
                                             ref-man-export-latex-table-no-caption-template
                                           ref-man-export-latex-table-template)
                                         (buffer-substring-no-properties
                                          (point)
                                          (progn (re-search-forward (rx "\\end{tabular}"))
                                                 (point))))
                            (kill-buffer (current-buffer)))))))
        (if current-prefix-arg
            (kill-new (format ref-man-export-latex-table-document-template table))
          (goto-char (org-table-end))
          (while (org-at-TBLFM-p)
            (forward-line))
          (insert "\n" table "\n")
          (org-indent-region (- (point) (length table)) (point))))
    (user-error "Not at an org table")))

(defun ref-man-export-parse-references (type &optional no-warn-types)
  "Parse the references from org text.
Search for all org links of type (or 'fuzzy 'custom-id 'http) and
gather the heading to which the link points to, if it can be
parsed as bibtex.

If TYPE is 'paper then buffer headings are collected as links, as
the buffer is narrowed to subtree by default.  Otherwise subtree
headings.

If a bibtex cannot be found for an org link a warning is raised.
To suppress the warning for given link types, optional
NO-WARN-TYPES can be passed as a list of (string) link types."
  (save-excursion
    (goto-char (point-min))
    ;; NOTE: If not paper, then collect subtree headings as sections, else
    ;;       buffer headings, as the buffer is already narrowed to DOC_ROOT.
    (let ((sections (if (eq type 'paper)
                        (util/org-apply-to-buffer-headings
                         (lambda () (concat "*" (org-get-heading t t t t))))
                      (util/org-apply-to-subtree-headings
                       (lambda () (concat "*" (org-get-heading t t t t))) t)))
          bibtexs)
      (goto-char (point-min))
      ;; NOTE: Generate bibtexs from org links only
      ;;
      ;; TODO: When encountering html links, we should either have a policy
      ;;       defined or prompt the user for one
      (while (re-search-forward util/org-text-link-re nil t nil)
        ;; NOTE: Don't insert link if it's of an internal section OR
        ;;       if we're in a comment
        ;; FIXME: How to fix for dup titles if they are for different papers (and bibs)?
        ;;        We should either report them as dups or store them with custom_ids
        (unless (or (member (match-string 1) sections)
                    (eq (org-element-type (org-element-context)) 'comment-block))
          (let ((bib (ref-man-org-get-bib-from-org-link t t t))
                (title-keys (mapcar (lambda (x) (-take 2 x)) bibtexs)))
            ;; NOTE: Append _a to duplicate bibtex key
            ;; TODO: Fix dups for CUSTOM_ID across org buffer
            (if (not (cadr bib))
                (let* ((link (org-element-context))
                       (link-type (org-element-property :type link)))
                  (unless (member link-type no-warn-types)
                    (warn "No bib found for match string %s and bib %s" (match-string 0) (car bib))))
              (when (-any #'identity (mapcar (lambda (x) (and title-keys
                                                              (string= (cadr x) (cadr bib))
                                                              (not (string= (car x) (car bib)))))
                                             title-keys))
                (setf (cadr bib) (concat (cadr bib) "_a"))
                (setf (nth 2 bib) (replace-regexp-in-string
                                   (string-remove-suffix "_a" (cadr bib))
                                   (cadr bib) (nth 2 bib))))
              (when (string-prefix-p "#" (car bib))
                (setf (car bib) (string-remove-prefix "#" (car bib))))
              (unless (member (cadr bib) (mapcar (lambda (x) (nth 1 x)) bibtexs))
                (push bib bibtexs))))))
      bibtexs)))

(defun ref-man-export-remove-src-blocks (&rest _args)
  "Remove source blocks by returning empty string."
  "")


(defun ref-man-export-generate-yaml-header (type abstract metadata refs-string)
  "Generate yaml header for `ref-man-export-docproc-article'.

TYPE is the export type.
ABSTRACT is used when type is 'paper.
METADATA is the metadata to be inserted in the markdown file.
REFS-STRING is references in yaml format."
  (replace-regexp-in-string
   "\n+" "\n"
   (concat "---\n"
           (if (eq type 'paper) (concat "\nabstract: >\n" abstract) "")
           (with-current-buffer
               (ref-man--post-json-synchronous (ref-man-py-url "get_yaml") metadata)
             (goto-char (point-min))
             (re-search-forward "\r?\n\r?\n")
             (concat (buffer-substring-no-properties (point) (point-max))  "\n"))
           (or refs-string "\n")
           "---")))

(defun ref-man-export-delete-md-toc (buf)
  "Delete TOC in a markdown buffer BUF."
  (with-current-buffer buf
    (goto-char (point-min))
    (let (toc-min toc-max)
      (when (re-search-forward "# Table of Contents" nil t))
      (beginning-of-line)
      (setq toc-min (point))
      (forward-paragraph 2)
      (setq toc-max (point))
      (delete-region toc-min toc-max))))

(defun ref-man-export-find-file-other-window-no-ask (file &optional noselect)
  "Find FILE in other window without any questions."
  (let ((win (ref-man--get-or-create-window-on-side))
          query-about-changed-file)
    (unless (string= (buffer-file-name (window-buffer win)) (f-filename file))
      (set-window-buffer win (if noselect
                                 (find-file-noselect file)
                               (find-file file))))))

(defun ref-man-export-extract-metadata (type plain)
  "Extract article metadata by running the required hooks.

TYPE is the article type.
PLAIN is only valid for TYPE 'paper and indicates
no journal specific formatting."
  (setq ref-man-export-metadata nil)
  (run-hook-with-args 'ref-man-export-metadata-hook type)
  (pcase type
    ('paper (unless plain
              (run-hook-with-args 'ref-man-export-journal-specific-metadata-hook type)))
    (_ nil)))


(defun ref-man-export-extract-research-highlights ()
  (let ((props (org-entry-properties)))
    `(("research-highlights" .
       ,(--> (a-get props "RESEARCH_HIGHLIGHTS")
             (split-string it ";")
             (mapcar 'string-trim it)
             (-filter (lambda (x) (not (string-empty-p x))) it))))))


(defun ref-man-export-do-housekeeping-bib-files (md-file checksum)
  "Perform housekeeping on existing bibliography files in output dir.

In case bibtexs have not changed, we don't need to generate a new
one.  So the new bib files are simply renamed with the checksum
of new tex file.

MD-FILE is the name of the markdown file.
CHECKSUM is the current data checksum."
  (let* ((md-dir (f-parent md-file))
         (files (f-files md-dir))
         (bbl-files (-sort (lambda (x y) (not (time-less-p (cdr x) (cdr y))))
                           (-filter (lambda (file) (or (f-ext? (car file) "bbl")
                                                       (f-ext? (car file) "blg")))
                                    (mapcar (lambda (file)
                                              `(,file . ,(file-attribute-modification-time
                                                          (file-attributes file))))
                                            files)))))
    (when bbl-files
      (seq-do
       (lambda (x)
         (let* ((ext (f-ext x))
                (filename (f-no-ext (f-filename x)))
                (old-checksum (-last-item (split-string filename "-")))
                (file-prefix (string-remove-suffix (concat "-" old-checksum) filename))
                (newname (concat file-prefix "-" (substring checksum 0 10) "." ext)))
           (unless (f-exists? (f-join md-dir newname))
             (copy-file x (f-join md-dir newname)))))
       (mapcar #'car (list (car bbl-files) (nth 1 bbl-files)))))))

(defun ref-man-export-fix-template (type metadata)
  (let* ((template (a-get metadata "template"))
         (template (or template
                       ;; NOTE: For blog there's a custom template and it's inserted separately
                       (pcase type
                         ('paper (ref-man-export-paper-template))
                         ('pdf (ref-man-export-pdf-template))
                         ('html (ref-man-export-html-template))
                         (_ nil)))))
    (when template
      ;; NOTE: When template is not a full path get path from `ref-man-export-templates'
      ;; CHECK: Why are we not simply setting it in yaml header?
      (if (f-exists? template)
          (format " --template=%s " template)
        (format " --template=%s " (a-get (ref-man-export-templates) template))))))


;; TODO: Exporting images directly as includegraphics in pdf
;;       1. Automatically remove "file://" prefix from file path in md
;;       2. Add ! before the image description
;;       3. Add pandoc extentions `implicit_figures' to generation

;; TODO: FIXME: the whole metadata thing is a mess. Fixit asap
;;       1. Currently metadata is generated first as defaults from
;;       `ref-man-export-get-opts' BUT, that doesn't process `article'
;;       but only the types enumerated in config.ini
;;       2. Then some extra options are added via `no-citeproc' and
;;       `no-cite-cmd'
;;       3. Then `ref-man-export-metadata-hook' is run but that again
;;       does not support `article' type but it does support `paper'.
;;
;;       IN FACT, there is an `ref-man-export-get-article-metadata'
;;       function attached to it by default, but that just seems to
;;       extract '(author keywords caption affiliations) from the
;;       property drawer AND runs only for `paper'.
;;
;;       AND, there's also `ref-man-export-get-paper-metadata' which
;;       is also run only for type `paper', which extracts abstract
;;       and points for rest of the sections beginning and end.
;;       4. 

;; TODO: Use a pndconf server instead
;; TODO: Export can take a while and it would be better to do
;;       it in an async process.
;; TODO: Should convert these args to keywords
;; TODO: org export opts should be a separate opts plist
(defun ref-man-export-docproc-article (buffer type &optional no-urls no-gdrive
                                              ox-opts-list no-cite force plain)
  "Export current org buffer subtree as an article.

Export to markdown first and then use pandoc and `pndconf' or
whatever its name is.

When argument BUFFER is non-nil, export the full buffer.  Default
is to export the subtree.

Argument TYPE specifies the output type, can be one of 'paper,
'pdf, 'html, 'both or 'blog.  There are subtle differences in
generation.  'pdf type is exported as an article without an
abstract.  In all types except 'paper, code blocks and tables are
preserved and exported without evaluation, while 'paper type
removes them.

Type 'both means both PDF and HTML files will be generated as
targets.  Type 'paper implies that output type is latex and pdf,
but that the first paragraph in the subtree is interpreted as the
abstract automatically.

Types 'blog and 'paper put additional metadata in the generated
files.  See `ref-man-export-blog-extra-opts'.

Optional non-nil NO-URLS means to not include URLs in generated
bibliography, similar for NO-GDRIVE and gdrive links.  Default is
to include both URLS and GDRIVE links.

Optional non-nil OX-OPTS-LIST can be used to change the default
org-export arguments.

When optional NO-CITE is non-nil, don't use CSL and citeproc.

Optional argument FORCE generates the file even if the content
hasn't changed.  The content is checked with `md5'.

Optional PLAIN is to indicate that the document should be
exported as a plain article and without any journal/conference
specific formatting.  Helpful for distributing drafts and
preprints."
  (unless (memq type '(blog paper html pdf both))
    (user-error "Type of generation must be one of [blog, paper, html, pdf, both]"))
  (let* ((org-export-with-broken-links (or (plist-get ox-opts-list :with-broken-links) 'mark)) ; mark broken links and they'll be replaced with citations
         (org-export-with-clocks (plist-get ox-opts-list :with-clocks))
         (org-export-with-date (plist-get ox-opts-list :with-date))
         (org-export-with-drawers (plist-get ox-opts-list :with-drawers))
         (org-export-with-latex (plist-get ox-opts-list :with-latex))    ; Ignore latex and pandoc will handle it
         (org-export-with-properties (plist-get ox-opts-list :with-properties))
         (org-export-with-sub-superscripts (plist-get ox-opts-list :with-sub-superscripts))
         (org-export-with-tables (plist-get ox-opts-list :with-tables))
         (org-export-with-tags (plist-get ox-opts-list :with-tags))
         (org-export-with-timestamps (plist-get ox-opts-list :with-timestamps))
         (org-export-with-todo-keywords (plist-get ox-opts-list :with-keywords))
         ;; NOTE: We need to keep this on else, for some reason, it generates an
         ;;       empty document for markdown. TOC is controlled with separate
         ;;       argument WITH-TOC below.
         (org-export-with-toc t)
         (org-export-filter-src-block-functions (plist-get ox-opts-list :with-src))
         (with-toc (plist-get ox-opts-list :with-tags))
         (pandoc ref-man-pandoc-executable)
         (templates-dir ref-man-export-pandoc-templates-dir)
         (tmp-bib-file (unless (or (string= "2.14" (ref-man-pandoc-version))
                                   (string< "2.14" (ref-man-pandoc-version)))
                         (make-temp-file "ref-bib-" nil ".bib")))
         (docs-dir ref-man-export-output-dir) ; where all docs are stored
         (config-file (or ref-man-export-pndconf-config-file ; NOTE: This is not used
                          (path-join docs-dir "config.ini")))
         (csl-dir ref-man-export-pandoc-csl-dir)
         (csl-file (pcase (org-entry-get (point) "CSL")
                     ('nil
                      (ref-man-export-csl-files
                       (if no-urls
                           ref-man-export-csl-no-urls-file
                         ref-man-export-csl-urls-file)))
                     (csl (ref-man-export-csl-files (downcase csl)))))
         (citeproc "biblatex")
         (mathjax-path ref-man-export-mathjax-dir)
         (pandocwatch "-m pndconf")
         (checksum (if buffer
                       (md5 (buffer-substring-no-properties (point-min) (point-max)))
                     (save-restriction
                       (org-narrow-to-subtree)
                       (md5 (buffer-substring-no-properties (point-min) (point-max))))))
         (python ref-man-export-python-executable)
         (title (substring-no-properties (org-get-heading t t t t)))
         (title-words (split-string (downcase (ref-man--remove-punc title t))))
         (out-dir (pcase type
                    ('blog ref-man-export-blog-dir)
                    (_ (path-join (if (org-entry-get (point) "MD_OUTPUT_DIR")
                                      (f-expand (org-entry-get (point) "MD_OUTPUT_DIR"))
                                    docs-dir)
                                  (string-join title-words "-")))))
         ;; CHECK: Should we still export `md-file' like this for blog
         ;;        etc. also?
         ;;
         ;;        We probably don't need as github can do version
         ;;        control by itself
         (md-file (pcase type
                    ('blog (path-join out-dir (concat (string-join title-words "-") ".md")))
                    (_ (path-join out-dir (concat (string-join (-take 3 title-words) "-")
                                                  "-" (substring checksum 0 10) ".md")))))
         (org-file (path-join out-dir (concat (string-join (-take 3 title-words) "-")
                                              "-" (substring checksum 0 10) ".org")))
         (out-file (pcase type
                     ((or 'pdf 'paper)
                      (path-join out-dir (concat (f-base md-file) ".pdf")))
                     ('html (path-join out-dir (concat (f-base md-file) ".html")))
                     (_ nil)))
         (metadata (ref-man-export-get-opts type title tmp-bib-file
                                            mathjax-path csl-file))
         (no-confirm (pcase (org-entry-get (point) "NO_CONFIRM" nil t)
                       ("t" t)
                       ("nil" nil)
                       (_ ref-man-export-no-confirm-overwrite)))
         (no-citeproc (pcase (org-entry-get (point) "NO_CITEPROC" nil t)
                        ("t" "--no-citeproc")
                        ("nil" "")
                        (_ "")))
         (no-cite-cmd (if no-cite "--no-cite-cmd" ""))
         (current-prefix-arg nil)
         (yaml-header "")
         (ref-man-export-metadata-hook
          ;; NOTE: Get journal specific metadata only if not exporting as plain
          (if plain
              ref-man-export-metadata-hook
            (-concat ref-man-export-metadata-hook '(ref-man-export-get-journal-metadata))))
         bibtexs template-opt)
    (when (and (f-exists? md-file) (f-exists? out-file) (not force)
               (not (eq type 'blog)))
      (ref-man-export-find-file-other-window-no-ask (or out-file md-file) out-file)
      (user-error "Text hasn't changed. Nothing to do here"))
    ;; NOTE: Get and merge the metadata
    (ref-man-export-extract-metadata type plain)
    (setq metadata (-filter #'cdr (a-merge metadata ref-man-export-metadata)))
    ;; NOTE fix template and maybe set template-opt
    (setq template-opt (ref-man-export-fix-template type metadata))
    (unless (f-exists? out-dir)
      (f-mkdir out-dir))
    (when (and (f-exists? md-file) (not no-confirm))
      (unless (y-or-n-p (format "The file with name %s exists.  Replace? "
                                (f-filename md-file)))
        (user-error "Aborted")))
    ;; NOTE: Some preprocessing, narrow to buffer and export to markdown in
    ;;       `ref-man-export-temp-md-buf'
    (save-restriction
      (save-mark-and-excursion
        (if buffer
            (when (eq type 'paper)
              (user-error "Cannot export type 'paper with full buffer"))
          (org-narrow-to-subtree))
        (pcase-let ((`(,abstract ,-bibtexs ,-metadata)
                     (ref-man-export-get-abstract-bibtexs type metadata)))
          (setq bibtexs -bibtexs)
          (setq metadata -metadata)
          (setq yaml-header (ref-man-export-generate-yaml-header
                             type abstract metadata
                             (ref-man-export-bibtexs
                              bibtexs citeproc tmp-bib-file no-gdrive))))
        (goto-char (point-min))
        (if (eq type 'paper)
            (ref-man-export-org-to-md type nil (and ref-man-export-paper-version-org-file org-file))
          (ref-man-export-org-to-md type))))
    ;; (save-restriction
    ;;   (save-mark-and-excursion
    ;;     ;; NOTE: Get bounds of article
    ;;     ;; FIXME: `buffer' may not be required for paper export.
    ;;     ;;        Is the code too intermingled?
    ;;     (unless buffer
    ;;       (org-narrow-to-subtree)
    ;;       (pcase type
    ;;         ('paper (let ((beg (a-get metadata "sections-beg"))
    ;;                       (end (a-get metadata "sections-end"))
    ;;                       (doc-root (a-get metadata "doc-root")))
    ;;                   ;; NOTE: When it's a paper, first paragraph is abstract and we
    ;;                   ;;       go to first subheading
    ;;                   (setq abstract (a-get metadata "abstract"))
    ;;                   (narrow-to-region beg end)
    ;;                   (setq metadata (a-dissoc metadata "abstract" "doc-root" "sections-beg" "sections-end"))))
    ;;         (_ nil)))
    ;;     (setq bibtexs (ref-man-export-parse-references type (a-get ref-man-export-bib-no-warn-types type)))
    ;;     (setq bibtexs (mapcar (lambda (x)
    ;;                             `(,(car x) ,(nth 1 x) ,(replace-regexp-in-string "venue=" "booktitle=" (nth 2 x))))
    ;;                           bibtexs))
    ;;     (setq yaml-header
    ;;           (ref-man-export-generate-yaml-header type abstract metadata
    ;;                                                (ref-man-export-bibtexs
    ;;                                                 bibtexs citeproc tmp-bib-file no-gdrive)))
    ;;     (goto-char (point-min))
    ;;     ;; NOTE: export to markdown in ref-man-export-temp-md-buf
    ;;     (if (eq type 'paper)
    ;;         (ref-man-export-org-to-md type nil (and ref-man-export-paper-version-org-file org-file))
    ;;       (ref-man-export-org-to-md type))))

    ;; NOTE: process markdown buffer
    (with-current-buffer ref-man-export-temp-md-buf
      ;; NOTE: Remove TOC if asked
      (unless with-toc
        (ref-man-export-delete-md-toc (current-buffer)))
      ;; NOTE: Repace "BROKEN LINK"s with bibtexs
      (goto-char (point-min))
      (while (re-search-forward "\\[BROKEN LINK: \\(.+?\\)]" nil t) ; Insert links
        (let* ((path (string-remove-prefix "\\" (match-string 1)))
               (bib (car (a-get bibtexs path))))
          (when bib (replace-match (format "[@%s]" bib)))))
      ;; NOTE: Replace \(\) math format as pandoc doesn't work with that
      (goto-char (point-min))
      (while (re-search-forward "\\\\(\\(.+\\)\\\\)" nil t)
        (replace-match "$\\1$"))
      ;; NOTE: Remove snippets in parentheses marked TODO
      ;;       Though perhaps it should remove until next paragraph
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (re-search-forward "(.*TODO.*)" nil t)
          (replace-match "")))
      ;; NOTE: Insert references heading WHEN there are references
      (when (string-match-p "\\[@[a-z0-9]+]" (buffer-string))
        (goto-char (point-max))
        (insert (if (eq type 'paper)
                    "\n\n# References\n"
                  "\n\n## References\n")))
      (goto-char (point-min))
      (insert (concat yaml-header "\n"))
      (write-region (point-min) (point-max) md-file))
    (when no-cite
      (ref-man-export-do-housekeeping-bib-files md-file checksum))
    ;; NOTE: Actual export
    (unless (eq type 'blog)
      (let* ((type (pcase type
                     ('both "html,pdf")
                     ('paper "pdf")
                     (_ type)))
             (cmd (string-join `(,(s-lex-format "cd ${docs-dir} && ${python} ${pandocwatch}")
                                 ,(s-lex-format "convert ${md-file} ${no-cite-cmd} --same-output-dir")
                                 ,(s-lex-format "-c ${config-file} ${no-citeproc} -g ${type}")
                                 ,(s-lex-format "--pandoc-path ${pandoc} --templates-dir ${templates-dir} --csl-dir ${csl-dir}")
                                 ,template-opt)
                               " "))
             (process-environment (mapcar
                                   (lambda (x) (split-string x "="))
                                   process-environment))
             (process-environment (mapcar (lambda (x) (string-join x "="))
                                          (a-assoc process-environment "PATH"
                                                   (list (concat (f-dirname ref-man-export-python-executable) ":"
                                                                 (car (a-get process-environment "PATH")))))))
             (msg (shell-command-to-string cmd))
             (buf (get-buffer-create "*pandoc and latex output*"))
             case-fold-search)
        (with-current-buffer buf
          (erase-buffer)
          (insert (format "Running command %s\n" cmd))
          (insert msg)
          (ansi-color-apply-on-region (point-min) (point-max))))
      (when tmp-bib-file
        (delete-file tmp-bib-file)))
    (ref-man-export-find-file-other-window-no-ask (or out-file md-file) out-file)))

;; TODO: This is buggy probably because of (util/org "util-org") kind of names
;; (defvar ref-man-export-async-paths
;;   (let ((matches (shell-command-to-string
;;                   (format "grep \"(require\" %s"
;;                           (string-join (-filter (lambda (x) (string-match-p "ref-man-.*\\.el$" x))
;;                                                 (f-files (f-parent load-file-name)))
;;                                        " "))))
;;         packages)
;;     (with-temp-buffer
;;       (insert matches)
;;       (goto-char (point-min))
;;       (while (re-search-forward "(require '\\(.+?\\))" nil t)
;;         (push (match-string 1) packages)))
;;     (-uniq
;;      (mapcar (lambda (x)
;;                (f-parent (find-library-name (string-trim (-last-item (split-string x)) "\"" "\""))))
;;              packages)))
;;   "All the paths required to be appended for async call.")

(provide 'ref-man-export)

;;; ref-man-export.el ends here
