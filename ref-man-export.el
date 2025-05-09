;;; ref-man-export.el --- Document export and publishing functionality for `ref-man'. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019,2020,2021,2022,2023,2025
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Saturday 26 April 2025 07:52:08 AM IST>
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

(defcustom ref-man-export-with-checksum '((pdf . nil) (blog . t) (paper . nil) (html . nil))
  "Should we export with checksum appended to title words?
It is an alist of publish types along with values."
  :type 'alist
  :group 'ref-man-export)

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

(defvar ref-man-export-metadata-hook '(ref-man-export-get-article-metadata)
  "Hook to run while exporting org properties as pandoc metadata.

The functions in this hook should modify
`ref-man-export-metadata' by appending to it an alist of any
custom metadata extraction method.

See `ref-man-export-get-article-metadata' for an example.")

(defvar ref-man-export-journal-specific-metadata-hook nil
  "Like `ref-man-export-metadata-hook' but for individual journals/venues.")

(defvar ref-man-export-metadata nil
  "Variable to gather results of `ref-man-export-metadata-hook'.")

(defcustom ref-man-export-pdflatex-env-vars ""
  "Environment variables for pdflatex."
  :type 'string
  :group 'ref-man)

(defcustom ref-man-export-paper-version-org-file nil
  "Save current org version file to disk for output type \\='paper.

When non-nil write an org file with md5 sum suffix also along
with markdown file.  Only for output type \\='paper."
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

(defvar ref-man-pandoc-bibtex-executable ref-man-pandoc-executable
  "Pandoc executable for converting bibtex to yaml.
Defaults to `ref-man-pandoc-executable'.")

(defvar ref-man-export-post-export-hook nil
  "Hook to run after export is done.")

;; TEST
;; (with-current-buffer "test.org"
;;   (let ((ref-man-export-no-confirm-overwrite t))
;;     (pcase (org-entry-get (point) "NO_CONFIRM" nil t)
;;       ("t" t)
;;       ("nil" nil)
;;       (_ ref-man-export-no-confirm-overwrite))))

(defun ref-man-pandoc-has-server ()
  "Pandoc bibtex executable is compiled with server.
Both the executable `ref-man-pandoc-bibtex-executable' and if the
server is running are checked."
  (and
   (string-match-p "\\+server"
                   (shell-command-to-string
                    (format "%s -v" ref-man-pandoc-bibtex-executable)))
   (condition-case nil
       (with-current-buffer (url-retrieve-synchronously "http://localhost:3030/version")
         (buffer-string))
     (error nil))))

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
(eval-when-compile
  (if (version< org-version "9.6.0")
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
                           (latex-environment . org-md-latex-environment)
                           (latex-fragment . org-md-latex-fragment)
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
                         (latex-environment . org-md-latex-environment)
                         (latex-fragment . org-md-latex-fragment)
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
        (:md-headline-style nil nil org-md-headline-style)
        (:md-toplevel-hlevel nil nil org-md-toplevel-hlevel)))))


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
written to it after `ref-man-export-pre-export-md-functions'
execution."
  (save-restriction
    (let ((buf-string (if (not subtree)
                          (buffer-string)
                        (org-narrow-to-subtree)
                        (buffer-string)))
          (link-re ref-man-maybe-file-fuzzy-custid-link-re)
          (backend (pcase type
                     ((or 'html 'pdf 'blog 'both 'ref-gfm) 'ref-gfm)
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
        ;; (when output-org-file
        ;;   (write-region (point-min) (point-max) output-org-file))
        (goto-char (point-min))
        (while (re-search-forward link-re nil t)
          (pcase-let ((`(,a ,b) (list (match-string 1) (match-string 2))))
            (replace-match
             (format "[[%s][%s]]" (replace-regexp-in-string "\\(?:file:\\)?.+::" "" a) b))))
        (run-hook-with-args 'ref-man-export-pre-export-md-functions)
        (org-export-to-buffer backend ref-man-export-temp-md-buf nil nil nil nil)
        (when output-org-file
          (write-file output-org-file))))))


(defun ref-man-export-get-abstract-bibtexs (type metadata with-abstract)
  "Get abstract and bibtexs from the buffer.

Org buffer may be possibly narrowed.

TYPE is the type of article being processed.
METADATA is the existing metadata.

METADATA is modified and returned in a list along with bibtexs and abstract."
  (let (abstract bibtexs)
    ;; NOTE: Get bounds of article
    (when (or (eq type 'paper) with-abstract)
      (let ((beg (a-get metadata "sections-beg"))
            (end (a-get metadata "sections-end")))
        ;; NOTE: When it's a paper, first paragraph is abstract and we
        ;;       go to first subheading
        (setq abstract (a-get metadata "abstract"))
        (narrow-to-region beg end)))
    (setq bibtexs (ref-man-export-parse-references type (a-get ref-man-export-bib-no-warn-types type)))
    (setq bibtexs (mapcar (lambda (x)
                            `(,(car x) ,(nth 1 x) ,(replace-regexp-in-string "venue=" "booktitle=" (nth 2 x))))
                          bibtexs))
    (setq metadata (a-dissoc metadata "abstract" "doc-root" "sections-beg" "sections-end"))
    (list abstract bibtexs metadata)))


;; FIXME: blog-opts is unused
(defun ref-man-export-get-opts (type title bib-file mathjax-path csl-file
                                            &optional no-refs)
  "Generate alist for yaml metadata for `ref-man-export-article'.

TYPE is one of \\='html \\='pdf \\='both \\='blog, TITLE is the title of the
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
    (pcase type
      ('blog
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
      ((or 'article 'pdf)
       (setq opts
             (a-assoc opts
                      "toc" (org-entry-get (point) "WITH_TOC")
                      "toc-depth" (or (org-entry-get (point) "TOC_DEPTH") 2)))))
    opts))


(defun ref-man-export-bibtex-to-yaml-via-shell (citeproc temp-file)
  "Get bibtex yaml metadata via executing pandoc shell command on TEMP-FILE.
CITEPROC is the citation processor to use with pandoc."
  (replace-regexp-in-string
   "^---\\|^nocite.*" ""
   (shell-command-to-string (format "%s -s -f %s -t markdown %s"
                                    ref-man-pandoc-executable
                                    citeproc
                                    temp-file))))


(defun ref-man-export-bibtex-to-yaml-via-pandoc-server (bib-text citeproc)
  "Get bibtex yaml metadata for BIB-TEXT via pandoc server.
CITEPROC is the citation processor to use with pandoc."
  (replace-regexp-in-string
   "^---\\|^nocite.*" ""
   (let* ((url "http://localhost:3030")
          (data `((text . ,bib-text)
                  (standalone . t)
                  (from . ,citeproc)
                  (to . markdown)))
          (url-request-extra-headers
           '(("Content-Type" . "application/json")))
          (url-request-method "POST")
          (url-request-data
           (encode-coding-string (json-encode data) 'utf-8)))
     (with-current-buffer (url-retrieve-synchronously url)
       (goto-char (point-min))
       (forward-paragraph)
       (buffer-substring (point) (point-max))))))


(defun ref-man-export-bib-strings-to-yaml-via-temp-file (citeproc)
  "Export bibtex strings BIBTEXS to yaml via `pandoc' and temp file.

CITEPROC is the citation processor.  One of `bibtex' or `biblatex'."
  (let ((temp-file (make-temp-file ".tmp-bib"))
        (cur (point-max)))
    (write-region (point-min) (point-max) temp-file)
    (goto-char cur)
    (insert (ref-man-export-bibtex-to-yaml-via-shell citeproc temp-file))
    (delete-file temp-file)
    (util/delete-blank-lines-in-region cur (point-max))
    (buffer-substring-no-properties cur (point-max))))


(defun ref-man-export-bib-strings (bibtexs citeproc &optional tmp-bib-file no-gdrive)
  "Export bibtex strings BIBTEXS to yaml or bib file.

CITEPROC is the citation processor to use.  Can be one of
`bibtex' or `biblatex'.

If optional TMP-BIB-FILE is given then write to that file.

Default is to export the BIBTEXS string as yaml metadata which is
read by `pandoc'.

Optional NO-GDRIVE implies to remove the gdrive keys if present."
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
            ;; Just use pandoc server if available
            ((ref-man-pandoc-has-server)
             (ref-man-export-bibtex-to-yaml-via-pandoc-server (buffer-string) citeproc))
            ;; Write to a temp file if \' accents are present
            ((string-match-p "'" (buffer-string))
             (ref-man-export-bib-strings-to-yaml-via-temp-file citeproc))
            (t
             (ref-man-export-bib-strings-to-yaml-via-temp-file citeproc))))))


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
    `((("name" . ,ref-man-export-author-name)
       ("email" . ,ref-man-export-author-email)
       ("affiliation" . ,ref-man-export-author-affiliation)))))

(defun ref-man-export-get-article-metadata (type)
  "Extract keywords and standard metadata for journal.
TYPE has to be \\='paper for this hook to run."
  (when (memq type '(paper pdf))
    (let* ((props (org-entry-properties))
           (author (ref-man-export-check-author (a-get props "AUTHOR")))
           (pandoc-metadata (mapcar (lambda (x) (pcase-let ((`(,a ,b) (split-string x ":")))
                                                  `(,a . ,b)))
                                    (split-string (or (a-get props "PANDOC_METADATA") "") ";" t "[ \n]+")))
           (affiliations (mapcar (lambda (y)
                                   (a-get ref-man-research-article-affiliations-alist y))
                                 (-uniq (mapcar
                                         (lambda (x) (a-get x "affiliation"))
                                         author)))))
      (setq ref-man-export-metadata
            (-concat ref-man-export-metadata
                     pandoc-metadata
                     `(("author" . ,author)
                       ("keywords" . ,(when (a-get props "KEYWORDS")
                                        (split-string (a-get props "KEYWORDS") ",")))
                       ("caption" . ,(a-get props "CAPTION"))
                       ("affiliations" . ,affiliations))
                     (ref-man-export-get-abstract-and-section-bounds))))))

(defun ref-man-export-get-journal-metadata (type)
  "Extract journal type and template from property drawer.
TYPE has to be \\='paper for this hook to run."
  (when (eq type 'paper)
    (let* ((props (org-entry-properties)))
      (setq ref-man-export-metadata
            (-concat ref-man-export-metadata
                     `(("journal" . ,(a-get props "JOURNAL"))
                       ("template" . ,(a-get props "TEMPLATE")))
                     (a-get ref-man-export-journal-args (a-get props "JOURNAL")))))))

(defun ref-man-export-get-abstract-and-section-bounds ()
  "Extract abstract and section demarcations for TYPE \\='paper."
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (pcase-let* ((`(,root ,before-refs) (ref-man-org-get-bounds-before-references))
                   (`(,beg ,end ,has-body) (progn (goto-char root)
                                                  (ref-man-org-text-bounds)))
                   (metadata))
        (when has-body
          (push (cons "abstract" (buffer-substring-no-properties beg end)) metadata))
        (a-assoc metadata
                 "doc-root" root
                 "sections-beg" end
                 "sections-end" before-refs)))))

(defun ref-man-export-get-all-imgs-subr (doc-root)
  (let (imgs)
    (save-excursion
      (if doc-root
          (goto-char doc-root)
        (goto-char (point-min)))
      (save-restriction
        (when doc-root
          (org-narrow-to-subtree))
        (while (re-search-forward "\\includegraphics\\(?:\\[.+?]\\){\\(.+?\\)}" nil t)
          (push (substring-no-properties (match-string 1)) imgs))))
    imgs))


(defun ref-man-export-get-all-imgs (&optional buffer-or-filename)
  "Return list of all file paths in `includegraphics' directives for org subtree.
Optional BUFFER specifies the org buffer."
  (let* ((buffer (unless (stringp buffer-or-filename)
                   (or buffer-or-filename (current-buffer))))
         (doc-root (when buffer (util/org-get-tree-prop "DOC_ROOT"))))
    (if buffer
        (with-current-buffer buffer
          (unless doc-root
            (user-error "For extraction of images for a research article, a DOC_ROOT must be specified"))
          (ref-man-export-get-all-imgs-subr doc-root))
      (let ((buffer (find-file-noselect buffer-or-filename)))
        (with-current-buffer buffer
          (prog1 (ref-man-export-get-all-imgs-subr nil)
            (kill-buffer)))))))


(defun ref-man-export-link-standalone-files (&optional tex-file out-dir)
  "Copy all supporting files corresponding to a TEX-FILE.

Create a new folder based on the title of the document and copy
the img, supporting tex and bib files for easy upload to
servers.

With a \\[universal-argument] the name is read from the
minibuffer, else it's determined from the \"title\" field of the
tex file."
  (interactive "p")
  (unless tex-file
    (error "Latex file must be given when calling non-interactively"))
  (let* ((interactive-p (numberp tex-file))
         (tex-file (or (and (not interactive-p) tex-file)
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
         (out-dir (or out-dir (f-join docs-dir (concat doc-name "-standalone/"))))
         (out-file (f-join out-dir (f-filename tex-file)))
         (out-bib-file (concat (string-remove-suffix ".tex" out-file) ".bib")))
    (kill-buffer buf)
    (unless (f-exists? out-dir)
      (f-mkdir out-dir))
    (copy-file tex-file out-file t)
    (when (find-buffer-visiting out-file)
      (kill-buffer (find-buffer-visiting out-file)))
    (make-symbolic-link bib-file out-bib-file t)
    (let ((buf (find-file-noselect out-file))
          cls-file sty-files img-files)
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "\\documentclass\\[.*?]{\\(.+?\\)}")
        (setq cls-file (substring-no-properties (match-string 1)))
        (maybe-delete-link out-dir (format "%s.cls" cls-file))
        (setq cls-file
              (string-trim (shell-command-to-string
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
        (make-symbolic-link cls-file out-dir t)
        (seq-do (lambda (x)
                  (maybe-delete-link out-dir (format "%s.sty" x))
                  (make-symbolic-link (string-trim
                                       (shell-command-to-string (format "kpsewhich %s.sty" x)))
                                      out-dir t))
                (-flatten sty-files))
        (seq-do (lambda (x)
                  (make-symbolic-link x out-dir t))
                img-files)
        (save-buffer))
      (kill-buffer buf))))


;; FIXME: compile-cmd is unused
(defun ref-man-export-generate-standalone-from-org ()
  "Create a folder from doc-root with all files required for a self contained document.

Copy the img, tex and bib files in the folder for easy upload to
servers.

Requires the bib and other files to be generated once with
`ref-man-export-article'."
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

;; FIXME: docs-dir and compile-cmd are unused
(defun ref-man-export-generate-standalone-from-latex ()
  "Create a folder from a latex file with all files required for a self contained document.

Copy the img, tex and bib files in the folder for easy upload to
servers.

Requires the bib and other files to be generated once."
  ;; NOTE: Perhaps output to a different directory also?
  (interactive)
  (let* ((file (ido-read-file-name "Enter the path of the LaTeX file "))
         (imgs (ref-man-export-get-all-imgs file))
         ;; (docs-dir ref-man-export-output-dir)
         (doc-name (f-base (f-filename file)))
         (existing-files-dir (f-parent file))
         (out-dir (concat existing-files-dir "-standalone/"))
         (files (mapcar (lambda (x) (path-join existing-files-dir (concat doc-name x)))
                        '(".tex" ".bib")))
         (default-directory existing-files-dir)
         ;; (compile-cmd (if current-prefix-arg
         ;;                  (s-lex-format "pdflatex ${doc-name}.tex && biber ${doc-name} && pdflatex ${doc-name}")
         ;;                (s-lex-format "pdflatex ${doc-name}.tex && bibtex ${doc-name}.aux && pdflatex ${doc-name}.tex && pdflatex ${doc-name}.tex && rm *.out *.aux *.log *.blg")))
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
  "Export BUFFER as \\='blog.

BUFFER defaults to `current-buffer'.

See `ref-man-export-article' for details."
  (interactive)
  (ref-man-export-article buffer 'blog t (not current-prefix-arg)
                                  '(:with-toc t :with-tables t)))

(defun ref-man-export-both-no-urls (&optional buffer)
  "Export BUFFER as both \\='blog and \\='pdf.

BUFFER defaults to `current-buffer'.

See `ref-man-export-article' for details."
  (interactive)
  (ref-man-export-article buffer 'both t (not current-prefix-arg)
                                  '(:with-toc t :with-tables t)))

(defun ref-man-export-html-no-urls (&optional buffer)
  "Export BUFFER as \\='html.

BUFFER defaults to `current-buffer'.

See `ref-man-export-article' for details."
  (interactive)
  (ref-man-export-article buffer 'html t (not current-prefix-arg)
                                  '(:with-toc t :with-tables t)
                                  nil current-prefix-arg))

(defun ref-man-export-article-no-urls-current-buffer-from-doc-root (doc-root)
  "Call interactively `ref-man-export-article-no-urls' from DOC-ROOT.
DOC-ROOT is a point from where subtree is to be exported."
  (save-excursion
    (goto-char doc-root)
    (call-interactively 'ref-man-export-article-no-urls)))

(defun ref-man-export-article-no-urls (pref-arg &optional buffer)
  "Export BUFFER as a pdf article.

If Optional BUFFER is non-nil then export the entire buffer, else
export only the subtree.

The output type depends on the prefix.
1. (default) Export buffer as PDF. Don't overwrite if output
   file exists.
2. Export buffer as HTML. Don't overwrite.
3. Export both HTML, PDF. Don't overwrite.
4. Export HTML. Overwrite.
5. Export both. Overwrite.

This function calls `ref-man-export-article' with
appropriate arguments."
  (interactive "p")
  (pcase pref-arg
    (1 (ref-man-export-article
        buffer 'pdf t t '(:with-tables t)))
    (2 (ref-man-export-article  ; html
        buffer 'html t t '(:with-tables t)))
    (3 (ref-man-export-article  ; both html pdf
        buffer 'both t t '(:with-tables t)))
    (4 (ref-man-export-article  ; force pdf
        buffer 'pdf t t '(:with-tables t) nil t))
    (5 (ref-man-export-article  ; force html
        buffer 'html t t '(:with-tables t) nil t))
    (6 (ref-man-export-article  ; force both
        buffer 'both t t '(:with-tables t) nil t))))

(defun ref-man-export-paper-no-urls (&optional pref-arg)
  "Export BUFFER as a research article.

Optional PREF-ARG reads the prefix argument as a number and if:
> 1 then don't use citeproc or bibtex or biber on PDF.
    Useful for repeated generation of PDFs when references
    don't need to be updated.
> 4 then force overwrite and don't run a citation command.

See `ref-man-export-article' for details."
  (interactive "p")
  (let ((doc-root (util/org-get-tree-prop "DOC_ROOT"))
        (no-cite (> pref-arg 1))
        (force (> pref-arg 4)))
    (unless doc-root
      (user-error "For generation of a research article, a DOC_ROOT must be specified"))
    (save-excursion
      (goto-char doc-root)
      (message "Exporting to PDF...")
      (ref-man-export-article nil 'paper t t
                              '(:with-src (ref-man-export-remove-src-blocks))
                              no-cite force))))

(defun ref-man-export-paper-plain-no-urls (&optional buffer)
  "Export BUFFER as a plain research article.

See `ref-man-export-paper-no-urls'."
  (interactive)
  (ref-man-export-article buffer 'paper t t '(:with-src (ref-man-export-remove-src-blocks))
                          nil nil t))

(defcustom ref-man-export-latex-table-template "\\begin{table}
\\centering
%s
\\caption{CAPTION}
\\label{tab:}
\\end{table}
"
  "Template for inserting table with `ref-man-export-org-table-to-latex'.
The table is inserted at %s and formatted with `format'."
  :type 'string
  :group 'ref-man)

(defcustom ref-man-export-latex-table-no-caption-template "\\begin{table}
\\centering
%s
\\end{table}
"
  "Template for inserting table with `ref-man-export-org-table-to-latex'.

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

(defvar ref-man-export-latex-table-properties nil
  "Plist of properties which will be parsed by `ref-man-export-format-latex-table'.

Options are:
:caption caption string
:caption-pos top or bottom
:center whether to center the table or not
:with-minipage use minipage
:label label for the table
:with-document insert \"\\begin,end{document}\" around it.

Example:

\(setq ref-man-export-latex-table-properties
      '(:caption-bottom t :center t :minipage nil))")

(defvar ref-man-export-latex-table-cleanup-hook nil)

(defun ref-man-export-format-latex-table (table-string table-opts)
  (let* ((caption (plist-get table-opts :caption))
         (caption-top (eq (plist-get table-opts :caption-pos) 'top))
         (caption-bottom (eq (plist-get table-opts :caption-pos) 'bottom))
         (center (plist-get table-opts :center))
         (minipage (plist-get table-opts :with-minipage))
         (label (plist-get table-opts :label))
         (with-document (plist-get table-opts :with-document))
         (text
          (format "%s
\\begin{table}
%s
%s
%s
%s
%s
\\end{table}
%s"
                  (if minipage "\\begin{minipage}{\\linewidth}" "")
                  (if center "\\centering" "")
                  (or (and caption-top (format "\\caption{%s}" caption)) "")
                  table-string
                  (if label (format "\\label{%s}" label) "")
                  (or (and caption-bottom (format "\\caption{%s}" caption)) "")
                  (if minipage "\\end{minipage}" "")))
         (text (with-temp-buffer
                 (insert text)
                 (util/delete-blank-lines-in-buffer nil t)
                 (buffer-string))))
    (if with-document
        (format ref-man-export-latex-table-document-template text)
      text)))

(defun ref-man-export-org-table-to-latex (&optional no-caption)
  "Export an org or table mode table in region to latex.

Do not add a caption if NO-CAPTION is non-nil."
  (interactive)
  (unless ref-man-export-latex-table-properties
    (user-error "You must set `ref-man-export-latex-table-properties' first"))
  (if (or (org-at-table-p) (org-at-table.el-p))
      (let* ((org-export-with-broken-links 'mark)
             (el (org-element-context))
             (el (progn (while (not (eq (car el) 'table))
                          (setq el (org-element-property :parent el)))
                        el))
             (caption (unless no-caption
                        (or (org-element-property :caption el)
                            (plist-get :caption (cadr el)))))
             (caption (and caption (caaar caption)))
             (label (or (org-element-property :label el) (org-element-property :name el)
                        (plist-get :label (cadr el)) (plist-get :name (cadr el))))
             (ref-man-export-latex-table-properties
              (-concat ref-man-export-latex-table-properties
                       `(:caption ,caption :label ,label)))
             org-export-show-temporary-export-buffer
             table)
        (util/save-mark-and-restriction
         (cond ((org-at-table-p)
                (narrow-to-region (org-table-begin) (org-table-end)))
               ((org-at-table.el-p)
                (let* ((cur (point))
                       (top (progn
                              (goto-char (car (-last-item (table--horizontal-cell-list))))
                              (goto-char (car (-last-item (table--vertical-cell-list))))
                              (forward-line -1)
                              (1- (point))))
                       (bottom (progn
                                 (goto-char cur)
                                 (goto-char (cdar (table--horizontal-cell-list)))
                                 (goto-char (cdar (table--vertical-cell-list)))
                                 (forward-line)
                                 (end-of-line)
                                 (point))))
                  (narrow-to-region top bottom))))
         (org-export-to-buffer 'latex "*Latex Export*" nil nil nil nil)
         (setq table (with-current-buffer "*Latex Export*"
                       (beginning-of-line)
                       (run-hook-with-args 'ref-man-export-latex-table-cleanup-hook)
                       (goto-char (point-min))
                       (re-search-forward (rx "\\begin{tabular}"))
                       (prog1 (ref-man-export-format-latex-table
                               (concat "\\begin{tabular}"
                                       (buffer-substring-no-properties
                                        (point)
                                        (progn (re-search-forward (rx "\\end{tabular}"))
                                               (point))))
                               ref-man-export-latex-table-properties)
                         (kill-buffer (current-buffer)))))
         (if current-prefix-arg
             (kill-new table)
           (cond ((org-at-table-p)
                  (goto-char (org-table-end))
                  (while (org-at-TBLFM-p)
                    (forward-line)))
                 ((org-at-table.el-p)
                  (table-goto-bottom-right-corner)
                  (end-of-line)))
           (insert "\n" table "\n")
           (org-indent-region (- (point) (length table)) (point)))))
    (user-error "Not at an org table")))

;; FIXME: match-1 match-2 are unused
(defun ref-man-export-parse-references (type &optional no-warn-types)
  "Parse the references from org text.
Search for all org links of type (or \\='fuzzy \\='custom-id \\='http) and
gather the heading to which the link points to, if it can be
parsed as bibtex.

If TYPE is \\='paper then buffer headings are collected as links, as
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
                      (-filter 'identity (util/org-apply-to-subtree-headings
                                          (lambda ()
                                            (let ((heading (substring-no-properties (org-get-heading t t t t))))
                                              (unless (string-match-p "references" heading)
                                                heading)))
                                          t))))
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
                    (save-match-data
                      (eq (org-element-type (org-element-context)) 'comment-block)))
          (let* ((match-0 (match-string 0))
                 (match-1 (match-string 1))
                 (match-2 (match-string 2))
                 (bib (ref-man-org-get-bib-from-org-link t t t))
                 (title-keys (mapcar (lambda (x) (-take 2 x)) bibtexs)))
            ;; NOTE: Append _a to duplicate bibtex key
            ;; TODO: Fix dups for CUSTOM_ID across org buffer
            (if (not (cadr bib))
                (let* ((link (org-element-context))
                       (link-type (org-element-property :type link)))
                  (unless (member link-type no-warn-types)
                    (warn "No bib found for match string %s and bib %s" match-0 (car bib))))
              (when (and (-any #'identity (mapcar
                                           (lambda (x) (and title-keys
                                                            (string= (cadr x) (cadr bib))
                                                            (not (string= (car x) (car bib)))))
                                           title-keys))
                         (not (equal (a-get bibtexs (cadr bib)) (cdr bib))))
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
  "Generate yaml header for `ref-man-export-article'.

TYPE is the export type.
ABSTRACT is used when type is \\='paper.
METADATA is the metadata to be inserted in the markdown file.
REFS-STRING is references in yaml format."
  (replace-regexp-in-string
   "\n+" "\n"
   (concat "---\n"
           (if abstract (concat "\nabstract: >\n" abstract) "")
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
      (when (re-search-forward "# Table of Contents" nil t)
        (beginning-of-line)
        (setq toc-min (point))
        (forward-paragraph 2)
        (setq toc-max (point))
        (delete-region toc-min toc-max)))))

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
PLAIN is only valid for TYPE \\='paper and indicates
no journal specific formatting."
  (setq ref-man-export-metadata nil)
  (run-hook-with-args 'ref-man-export-metadata-hook type)
  (run-hook-with-args 'ref-man-export-journal-specific-metadata-hook type))

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
                           (-filter (lambda (file)
                                      (string-match-p ".+\\(?:\\.bib\\|\\.blg\\|\\.bbl\\)$"
                                                      (car file)))
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
       ;; NOTE: In case we want a bib file also copied
       ;; (mapcar #'car (cons (-first (lambda (x) (string-match-p ".+\\.bib$" (car x)))
       ;;                                    bbl-files)
       ;;                            (-take 2 bbl-files)))
       (mapcar #'car (-take 2 bbl-files))))))

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

(defun ref-man-export-narrow-to-references ()
  "Narrow from beginning of buffer to References in an org subtree."
  (save-excursion
    (goto-char (point-min))
    (let ((beg (point)))
      (while (and (outline-next-heading)
                  (not (string= (substring-no-properties
                                 (downcase (org-get-heading t t t t)))
                                "references"))))
      (narrow-to-region beg (point)))))


(defun ref-man-export-to-md (pref-arg)
  (interactive "p")
  (let ((doc-root (util/org-get-tree-prop "DOC_ROOT")))
    (save-excursion
      (when doc-root
        (goto-char doc-root))
      (let* ((with-toc (= pref-arg 4))
             (out-dir (if (org-entry-get (point) "MD_OUTPUT_DIR")
                          (f-expand (org-entry-get (point) "MD_OUTPUT_DIR"))
                        (ido-read-directory-name "Enter the name of the output directory: ")))
             (title (substring-no-properties (org-get-heading t t t t)))
             (title-words (split-string (downcase (ref-man--remove-punc title t))))
             (md-file (f-join out-dir (concat (string-join title-words "-") ".md"))))
        (ref-man-export-org-to-md 'ref-gfm t)
        (with-current-buffer ref-man-export-temp-md-buf
          (unless with-toc
            (ref-man-export-delete-md-toc (current-buffer)))
          (write-region (point-min) (point-max) md-file))
        (ref-man-export-find-file-other-window-no-ask md-file t)))))

(defun ref-man-export-determine-output-dir (type docs-dir title-words)
  (let ((out-file (org-entry-get (point) "OUTFILE")))
    (if out-file
        (f-dirname out-file)
      (pcase type
        ('blog ref-man-export-blog-dir)
        (_ (path-join (if (org-entry-get (point) "MD_OUTPUT_DIR")
                          (f-expand (org-entry-get (point) "MD_OUTPUT_DIR"))
                        docs-dir)
                      (string-join title-words "-")))))))

(defun ref-man-export-determine-md-file (type out-dir title-words checksum with-checksum)
  (let ((out-file (org-entry-get (point) "OUTFILE")))
    (if (and out-file (string-suffix-p ".md" out-file))
        out-file
      (pcase type
        ('blog (path-join out-dir (concat (string-join title-words "-") ".md")))
        (_ (path-join out-dir (concat
                               (string-join (if with-checksum
                                                (-take 3 title-words)
                                              title-words) "-")
                               (if with-checksum
                                   (concat "-" (substring checksum 0 10))
                                 "")
                               ".md")))))))

(defun ref-man-export-determine-out-file (type out-dir md-file)
  (let ((out-file (org-entry-get (point) "OUTFILE")))
    (or out-file
        (pcase type
          ((or 'pdf 'paper)
           (path-join out-dir (concat (f-base md-file) ".pdf")))
          ('html (path-join out-dir (concat (f-base md-file) ".html")))
          (_ nil)))))

(defun ref-man-export-check-with-checksum (type)
  (let ((with-checksum (a-get ref-man-export-with-checksum type))
        (option (and (a-get (org-entry-properties) "WITH_CHECKSUM")
                     (org-entry-get (point) "WITH_CHECKSUM"))))
    (if option option with-checksum)))

(defun ref-man-org-buffer-or-file-checksum (buffer)
  (if buffer
      (md5 (buffer-substring-no-properties (point-min) (point-max)))
    (save-restriction
      (org-narrow-to-subtree)
      (md5 (buffer-substring-no-properties (point-min) (point-max))))))


(defun ref-man-export-title (buffer)
  (if buffer
      (substring-no-properties (or (org-get-heading t t t t)
                                   "Org Buffer"))
    (substring-no-properties (org-get-heading t t t t))))

(defun ref-man-export-get-csl-file (no-urls)
  (pcase (org-entry-get (point) "CSL")
    ('nil
     (ref-man-export-csl-files
      (if no-urls
          ref-man-export-csl-no-urls-file
        ref-man-export-csl-urls-file)))
    (csl (ref-man-export-csl-files (downcase csl)))))

(defun ref-man-export-write-md-file (md-file type with-toc yaml-header bibtexs)
  "Fix the markdown export buffer and write to MD-FILE.

TYPE is the type of file of final export.

WITH-TOC signifies to leave the table of contents at the
top. Default is to not leave it there.

YAML-HEADER is the yaml metadata which will be processed by pandoc.

BIBTEXS is the list of extracted bibtexs.
See `ref-man-export-parse-references' for how bibtexs are extracted."
  (with-current-buffer ref-man-export-temp-md-buf
    ;; NOTE: Remove TOC if asked
    (unless with-toc
      (ref-man-export-delete-md-toc (current-buffer)))
    ;; NOTE: Repace "BROKEN LINK"s with bibtexs
    (goto-char (point-min))
    (while (re-search-forward "\\[BROKEN LINK: \\(.+?\\)]" nil t) ; Insert links
      (let* ((path (string-remove-prefix "\\" (match-string 1)))
             (bib (car (a-get bibtexs path))))
        (when bib (replace-match (format "[@%s]" bib))))) ; "\\cite{%s}"
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
    (when (string-match-p "\\[@[a-z0-9]+]" (buffer-string)) ; "\\\\cite{[a-z0-9}+]"
      (goto-char (point-max))
      (insert (if (eq type 'paper)
                  "\n\n# References\n"
                "\n\n## References\n")))
    (goto-char (point-min))
    (insert (concat yaml-header "\n"))
    (let ((coding-system-for-write 'raw-text))
      (write-region (point-min) (point-max) md-file))))


(defun ref-man-export-generate-output-file-with-pndconf
    (type md-file tmp-bib-file template-opt pndconf-opts)
  "Write the output file.

The output file is determined by the TYPE.

MD-FILE is converted with `pndconf' to the desired output file.

A TMP-BIB-FILE may be used to generate the bibliography depending
on the options given.

TEMPLATE-OPT is a fixed path to the required template.

PNDCONF-OPTS are the options required for generating the document.
These options are set in `ref-man-export-article'."
  (let ((python (a-get pndconf-opts 'python))
        (pandoc (a-get pndconf-opts 'pandoc))
        (pndconf (a-get pndconf-opts 'pndconf))
        (config-file (a-get pndconf-opts 'config-file))
        (no-cite-cmd (a-get pndconf-opts 'no-cite-cmd))
        (no-citeproc (a-get pndconf-opts 'no-citeproc)))
    (unless (eq type 'blog)
      (let* ((type (pcase type
                     ('both "html,pdf")
                     ('paper "pdf")
                     (_ type)))
             (cmd (string-join `(,(s-lex-format "${python} ${pndconf}")
                                 ,(s-lex-format "--pandoc-path ${pandoc} -c ${config-file}")
                                 ,(s-lex-format "convert ${md-file} ${no-cite-cmd} --same-pdf-output-dir")
                                 ,(s-lex-format "${no-citeproc} -g ${type}")
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
          (setq-local show-trailing-whitespace nil)
          (erase-buffer)
          (insert (format "Running command %s\n" cmd))
          (insert msg)
          (ansi-color-apply-on-region (point-min) (point-max))))
      (when tmp-bib-file
        (delete-file tmp-bib-file)))))

;; FIXME: Lexical vars templates-dir csl-dir pandoc-opts are not used

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

;; TODO: Use a pndconf server instead
;; TODO: Export can take a while and it would be better to do
;;       it in an async process.
;; TODO: Should convert these args to keywords
;; TODO: org export opts should be a separate opts plist
(defun ref-man-export-article (bufferp type &optional no-urls no-gdrive
                                       ox-opts-list no-cite force plain)
  "Export current org buffer or subtree as an article.

Export to markdown first and then use `pandoc' and `pndconf'.

When argument BUFFERP is non-nil, export the full buffer.  Default
is to export the subtree.

Argument TYPE specifies the output type, can be one of \\='paper,
\\='pdf, \\='html, \\='both or \\='blog.  There are subtle differences in
generation.  \\='pdf type is exported as an article without an
abstract.  In all types except \\='paper, code blocks and tables are
preserved and exported without evaluation, while \\='paper type
removes them.

Type \\='both means both PDF and HTML files will be generated as
targets.  Type \\='paper implies that output type is latex and pdf,
but that the first paragraph in the subtree is interpreted as the
abstract automatically.

Types \\='blog and \\='paper put additional metadata in the generated
files.  See `ref-man-export-blog-extra-opts'.

Optional non-nil NO-URLS means to not include URLs in generated
bibliography.  Optional NO-GDRIVE acts similarly for gdrive
links.  Default is to include both URLS and GDRIVE links.

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
  (let*
      ((org-export-with-broken-links
        (or (plist-get ox-opts-list :with-broken-links) 'mark)) ; mark broken links and they'll be replaced with citations
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
       org-export-with-toc
       (org-export-filter-src-block-functions (plist-get ox-opts-list :with-src))
       (with-toc (or (org-entry-get (point) "WITH_TOC")
                     (plist-get ox-opts-list :with-toc)))
       (with-abstract (org-entry-get (point) "WITH_ABSTRACT"))
       ;; NOTE: pndconf yaml opts
       (templates-dir ref-man-export-pandoc-templates-dir)
       (tmp-bib-file (unless (or (string= "2.14" (ref-man-pandoc-version))
                                 (string< "2.14" (ref-man-pandoc-version)))
                       (make-temp-file "ref-bib-" nil ".bib")))
       (docs-dir ref-man-export-output-dir) ; where all docs are stored
       (csl-dir ref-man-export-pandoc-csl-dir)
       (csl-file (ref-man-export-get-csl-file no-urls))
       (citeproc "biblatex")
       (mathjax-path ref-man-export-mathjax-dir)
       ;; NOTE: pndconf opts
       (pndconf-cmdline-opts
        `((python . ,ref-man-export-python-executable)
          (pandoc . ,ref-man-pandoc-executable)
          (pndconf . "-m pndconf")
          (config-file . ,(or ref-man-export-pndconf-config-file ; NOTE: This is not used
                           (path-join docs-dir "config.ini")))
          (no-cite-cmd . ,(if no-cite "--no-cite-cmd" ""))
          (no-citeproc . ,(pcase (org-entry-get (point) "NO_CITEPROC" nil t)
                            ("t" "--no-citeproc")
                            ("nil" "")
                            (_ "")))))
       (checksum (ref-man-org-buffer-or-file-checksum bufferp))
       (with-checksum (ref-man-export-check-with-checksum type))
       (title (ref-man-export-title bufferp))
       (title-words (split-string (downcase (ref-man--remove-punc title t))))
       (out-dir (ref-man-export-determine-output-dir type docs-dir title-words))
       (md-file (ref-man-export-determine-md-file type out-dir title-words checksum with-checksum))
       (out-file (ref-man-export-determine-out-file type out-dir md-file))
       (org-file (path-join out-dir (concat (string-join (-take 3 title-words) "-")
                                            "-" (substring checksum 0 10) ".org")))
       (metadata (ref-man-export-get-opts type title tmp-bib-file
                                          mathjax-path csl-file))
       (force (or force (org-entry-get (point) "FORCE")))
       (no-confirm (pcase (org-entry-get (point) "NO_CONFIRM" nil t)
                     ("t" t)
                     ("nil" nil)
                     (_ ref-man-export-no-confirm-overwrite)))
       (current-prefix-arg nil)
       (yaml-header "")
       (ref-man-export-metadata-hook
        ;; NOTE: Get journal specific metadata only if not exporting as plain
        (if (and (eq type 'paper) (not plain))
            (-concat ref-man-export-metadata-hook
                     '(ref-man-export-get-journal-metadata))
          ref-man-export-metadata-hook))
       (pandoc-opts (when-let ((opts (org-entry-get (point) "PANDOC_OPTS")))
                      (split-string opts "," t "[ \t\n\r]+")))
       bibtexs template-opt)

    ;; Don't do anything if content is same, unless force is non-nil
    (when (and with-checksum (f-exists? md-file) (f-exists? out-file) (not force)
               (not (eq type 'blog)))
      (ref-man-export-find-file-other-window-no-ask (or out-file md-file) out-file)
      (user-error "Text hasn't changed. Nothing to do here"))

    ;; Get and merge the metadata
    (ref-man-export-extract-metadata type plain)
    (setq metadata (-filter #'cdr (a-merge metadata ref-man-export-metadata)))

    ;; Fix template and maybe set template-opt
    (setq template-opt (ref-man-export-fix-template type metadata))

    (unless (f-exists? out-dir) (f-mkdir out-dir))

    (when (and (f-exists? md-file) (not no-confirm))
      (unless (y-or-n-p (format "The file with name %s exists.  Replace? "
                                (f-filename md-file)))
        (user-error "Aborted")))

    ;; Some preprocessing, narrow to buffer and export to markdown in
    ;; `ref-man-export-temp-md-buf'
    (save-restriction
      (save-mark-and-excursion
        (if bufferp
            (when (eq type 'paper)
              (user-error "Cannot export type 'paper with full buffer"))
          (org-narrow-to-subtree))
        (pcase-let ((`(,abstract ,-bibtexs ,-metadata)
                     (ref-man-export-get-abstract-bibtexs type metadata with-abstract)))
          (setq bibtexs -bibtexs)
          (setq metadata -metadata)
          (setq yaml-header (ref-man-export-generate-yaml-header
                             type (when (or with-abstract (eq type 'paper)) abstract) metadata
                             (ref-man-export-bib-strings
                              bibtexs citeproc tmp-bib-file no-gdrive))))
        (goto-char (point-min))
        (if (eq type 'paper)
            (ref-man-export-org-to-md type nil (and ref-man-export-paper-version-org-file org-file))
          ;; Don't export References, they'll be added automatically
          (ref-man-export-narrow-to-references)
          (ref-man-export-org-to-md type))))

    ;; Process markdown buffer and write it
    (ref-man-export-write-md-file md-file type with-toc yaml-header bibtexs)
    (when no-cite
      (ref-man-export-do-housekeeping-bib-files md-file checksum))

    ;; Actual export to target type, unless only markdown was required
    (unless (string= out-file md-file)
      (ref-man-export-generate-output-file-with-pndconf
       type md-file tmp-bib-file template-opt pndconf-cmdline-opts))
    (run-hook-with-args 'ref-man-export-post-export-hook
                        :bibtexs bibtexs :metadata metadata
                        :yaml-header yaml-header :md-file md-file)
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
