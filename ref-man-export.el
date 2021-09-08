;;; ref-man-export.el --- Core Components for `ref-man'. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019,2020,2021
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Thursday 09 September 2021 01:23:06 AM IST>
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
;; We use `pandoc' for exporting. Currently we export from an `org-mode'
;; buffers but other modes can also be handled easily.
;;
;; We use markdown as intermediate format for exporting as then it can be used
;; by many other frameworks and `pandoc' has good support for embedding extra
;; metadata in markdown. While data like username and email can be setup easily
;; in `org', we also generate the bibliography directly from org text
;; links. Which means we either keep temporary .bib files corresponding to each
;; generated file or embed the bibliography in the metadata. Putting all that
;; data inside the org properties or any other drawer would be redundant, so we
;; include that information in the yaml header in the markdown file.

;;; Code:

(require 'util)
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
  "Name of the author for export articles"
  :type 'string
  :group 'ref-man)

;; TODO: Import from an alist like `ref-man-export-research-article-args'
(defcustom ref-man-export-yaml-template "---
title: %s
author: %s
%s
link-citations: true
mathjax: %s
csl: %s
---
"
  "Template for yaml header for `ref-man-export-docproc-article'"
  :type 'string
  :group 'ref-man)

(defcustom ref-man-export-blog-yaml-template "---
title: %s
author: %s
%s
link-citations: true
mathjax: %s
csl: %s
date: %s
category: %s
tags: %s
keywords: %s
draft: true
---
"
  "Template for yaml header for blog for export with `ref-man-export-docproc-article'"
  :type 'string
  :group 'ref-man)

(defcustom ref-man-export-research-article-args nil
  "Extra arguments for exporting a research article.
Should be an `alist' parseable by `yaml-encode'."
  :type 'alist
  :group 'ref-man)

(defcustom ref-man-export-docproc-dir ""
  "Directory of docproc"
  :type 'directory
  :group 'ref-man)

(defcustom ref-man-export-mathjax-dir ""
  "Directory of docproc"
  :type 'directory
  :group 'ref-man)

(defcustom ref-man-export-csl-urls-file nil
  "Path to csl file which would allow including urls."
  :type 'file
  :group 'ref-man)

(defcustom ref-man-export-csl-no-urls-file nil
  "Path to csl file which would not allow including urls."
  :type 'file
  :group 'ref-man)

(defvar ref-man-export-md-buffer-name " *ref-man-export-md-buf*")

(defcustom ref-man-export-pdflatex-env-vars ""
  "Environment variables for pdflatex."
  :type 'string
  :group 'ref-man)

(defun ref-man-export-templates ()
  "Get templates as an alist from `ref-man-export-pandoc-templates-dir'."
  (mapcar
   (lambda (x) (cons
                (downcase (replace-regexp-in-string "\\.template\\|default\\." ""
                                                    (f-filename x)))
                x))
   (f-files ref-man-export-pandoc-templates-dir)))

(defun ref-man-export-csl-files ()
  "Get csl files as an alist from `ref-man-export-pandoc-csl-dir'."
  (mapcar
   (lambda (x) (cons
                (downcase (string-remove-suffix ".csl" (f-filename x)))
                x))
   (-filter (lambda (x) (string-suffix-p ".csl" (f-filename x)))
            (f-files ref-man-export-pandoc-csl-dir))))

;; TODO: This should be a macro
(defun ref-man-export-pdf-template ()
  (concat "--template=" (a-get (ref-man-export-templates)
                               (or (org-entry-get (point) "TEMPLATE")) "latex")))

(defun ref-man-export-paper-template ()
  (concat "--template=" (a-get (ref-man-export-templates)
                               (or (org-entry-get (point) "TEMPLATE") "ieee"))))

(defun ref-man-export-html-template ()
  (concat "--template=" (a-get (ref-man-export-templates)
                               (or (org-entry-get (point) "TEMPLATE") "blog"))))

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

(defun ref-man-export-org-to-md (&optional pre-export-func subtree)
  "Copy the org buffer to temp buffer and export to markdown.
We replace all file links so that they'll be subbed with citation
formats later.  Optional PRE-EXPORT-FUNC is called just before
calling `org-export-to-buffer'."
  (save-restriction
    (let ((buf-string (if (not subtree)
                          (buffer-string)
                        (org-narrow-to-subtree)
                        (buffer-string)))
          org-export-show-temporary-export-buffer)
      (with-temp-buffer
        (insert buf-string)
        (org-mode)
        (goto-char (point-min))
        (while (re-search-forward org-link-bracket-re nil t nil)
          (replace-match
           (format "[[%s][%s]]" (replace-regexp-in-string "file:.+::" "" (match-string 1))
                   (match-string 2))))
        (when pre-export-func (funcall pre-export-func))
        (org-export-to-buffer 'ref-md ref-man-export-md-buffer-name nil nil nil nil)))))

(defun ref-man-export-generate-yaml-header (type title bib-file mathjax-path csl-file
                                            &optional no-refs)
  "Generate yaml metadata header for `ref-man-export-docproc-article'.
TYPE is one of 'html 'pdf 'both 'blog, TITLE is the title of the
article, BIB-FILE is the additional bibliography file for
citations, MATHJAX-PATH is the path where Mathjax scripts would
be located and CSL-FILE is the Citation Style File."
  (let ((yaml-header ref-man-export-yaml-template)
        (blog-header ref-man-export-blog-yaml-template)
        (author ref-man-export-author-name))
    (pcase type
      ((or 'html 'pdf 'both 'paper)
       (format yaml-header title author
               (if no-refs ""
                 (ref-man-pandoc-bib-string bib-file))
               mathjax-path csl-file))
      ('blog (format blog-header title author (ref-man-pandoc-bib-string bib-file)
                     mathjax-path csl-file (time-stamp-string "%Y-%m-%d")
                     (or (util/org-get-tree-prop "CATEGORY_ROOT" t)
                         (downcase (ref-man-org-ask-to-set-property "BLOG_CATEGORY")))
                     (or (org-get-tags nil t)
                         (downcase (ref-man-org-ask-to-set-property "BLOG_TAGS")))
                     (downcase (or (ref-man-org-ask-to-set-property "BLOG_KEYWORDS")
                                   (ref-man-org-ask-to-set-property "BLOG_TAGS"))))))))

(defun ref-man-export-docproc-references (bibtexs &optional tmp-bib-file no-gdrive)
  "Export the references for docproc."
  (when bibtexs
    (with-temp-buffer
      (insert (mapconcat (lambda (x) (nth 2 x)) bibtexs ""))
      (unless no-gdrive
        (goto-char (point-min))
        ;; Replace "gdrive" with "issn" in bibs
        (while (re-search-forward "gdrive=" nil t nil)
          (replace-match "issn=")))
      ;; NOTE: Write to a temp file if \' accents are present
      (cond (tmp-bib-file
             (write-region (point-min) (point-max) tmp-bib-file))
            ((string-match-p "'" (buffer-string))
             (let ((temp-file (make-temp-file ".tmp-bib"))
                   (cur (point-max)))
               (write-region (point-min) (point-max) temp-file)
               (goto-char cur)
               (insert (replace-regexp-in-string
                        "^---\\|^nocite.*" ""
                        (shell-command-to-string (format "%s -s -f biblatex -t markdown %s"
                                                         ref-man-pandoc-executable
                                                         temp-file))))
               (delete-file temp-file)
               (util/delete-blank-lines-in-region cur (point-max))
               (buffer-substring-no-properties cur (point-max))))
            (t
             (let ((cur (point-max)))
               (goto-char cur)
               (insert (replace-regexp-in-string
                        "^---\\|^nocite.*" ""
                        (shell-command-to-string (format "%s -s -f biblatex -t markdown <<<'%s'"
                                                         ref-man-pandoc-executable
                                                         (buffer-string)))))
               (util/delete-blank-lines-in-region cur (point-max))
               (buffer-substring-no-properties cur (point-max))))))))

(defun ref-man-export-blog-no-urls (&optional buffer)
  (interactive)
  (ref-man-export-docproc-article buffer 'blog t (not current-prefix-arg)))

(defun ref-man-export-both-no-urls (&optional buffer)
  (interactive)
  (ref-man-export-docproc-article buffer 'both t (not current-prefix-arg)))

(defun ref-man-export-html-no-urls (&optional buffer)
  (interactive)
  (ref-man-export-docproc-article buffer 'html t (not current-prefix-arg)))

(defun ref-man-export-article-no-urls (&optional buffer type)
  (interactive)
  (ref-man-export-docproc-article buffer (or type (and current-prefix-arg 'html) 'pdf) t))

(defun ref-man-export-paper-no-urls (&optional buffer)
  (interactive)
  (let ((doc-root (util/org-get-tree-prop "DOC_ROOT")))
    (unless doc-root
      (user-error "For generation of a research article, a DOC_ROOT must be specified."))
    (save-excursion
      (goto-char doc-root)
      (ref-man-export-docproc-article buffer 'paper t t))))

(defun ref-man-org-export-table-to-latex ()
  (interactive)
  (save-excursion
    (save-restriction
      (let ((org-export-with-broken-links 'mark)
            org-export-show-temporary-export-buffer)
        (org-mark-element)
        (narrow-to-region (region-beginning) (region-end))
        (org-export-to-buffer 'latex "*Latex Export*" nil nil nil nil)
        (with-current-buffer "*Latex Export*"
          (goto-char (point-min))
          (re-search-forward (rx "\\begin{tabular}"))
          (beginning-of-line)
          (kill-new (concat "\\begin{minipage}{\\linewidth}\n"
                            "\\captionof{table}{CAPTION} \\label{}\n"
                            (buffer-substring-no-properties (point)
                                                            (progn (re-search-forward (rx "\\end{tabular}"))
                                                                   (point)))
                            "\n\\end{minipage}\n"))
          (kill-buffer (current-buffer))))))
  (when mark-active
    (deactivate-mark)))

;; TODO: Use a docproc server instead
(defun ref-man-export-docproc-article (buffer type &optional no-urls no-gdrive with-toc)
  "Export current org buffer subtree as an article.

Export to markdown first and then use pandoc and `pndconf' or
`docproc' whatever its name is.  When optional BUFFER is non-nil,
export the full buffer.  Default is export the subtree.

Optional TYPE specifies the output type, can be one of 'paper,
'pdf, 'html, 'both or 'blog.  'both means both PDF and HTML files
will be generated as targets.

'paper implies that output type is latex and pdf, but that the
first paragraph in the subtree is interpreted as the abstract
automatically.

Types 'blog and 'paper put additional metadata in the generated files.
See `ref-man-export-blog-yaml-template'.

Optional non-nil NO-URLS means to not include URLs in generated
bibliography, similar for NO-GDRIVE. Default is to include both
URLS and GDRIVE links.  Optional non-nil WITH-TOC generates a TOC
from `org-export'.  Usually we generate the TOC with pandoc."
  (let* ((org-export-with-toc t)
         (org-export-with-todo-keywords nil)
         (org-export-with-tags nil)
         (org-export-with-broken-links 'mark)
         (org-export-with-timestamps nil)
         (org-export-with-clocks nil)
         (org-export-with-tables nil)
         (org-export-with-sub-superscripts nil)
         (org-export-with-date nil)
         (org-export-with-properties nil)
         (org-export-with-latex nil)
         (tmp-bib-file (unless (or (string= "2.14" ref-man-pandoc-version)
                                   (string< "2.14" ref-man-pandoc-version))
                         (make-temp-file "ref-bib-" nil ".bib")))
         (docproc-dir ref-man-export-docproc-dir)
         (csl-file (pcase (org-entry-get (point) "CSL")
                     ('nil
                      (if no-urls
                          ref-man-export-csl-no-urls-file
                        ref-man-export-csl-urls-file))
                     (csl (a-get (ref-man-export-csl-files) (downcase csl)))))
         (mathjax-path ref-man-export-mathjax-dir)
         (pandocwatch (path-join docproc-dir "pandocwatch.py"))
         (python "/usr/bin/python")
         (title (substring-no-properties (org-get-heading t t t t)))
         (md-file (path-join (pcase type
                               ('blog ref-man-export-blog-dir)
                               (_ docproc-dir))
                             (concat (replace-regexp-in-string
                                      " " "-"
                                      (downcase (ref-man--remove-punc title t)))
                                     ".md")))
         (out-file (pcase type
                     ((or 'pdf 'paper)
                      (path-join docproc-dir (concat (f-base md-file) "_files")
                                 (concat (f-base md-file) ".pdf")))
                     ('html (path-join docproc-dir (concat (f-base md-file) ".html")))
                     (_ nil)))
         (extra-opts (pcase type
                       ('paper (concat " " (ref-man-export-paper-template) " "))
                       ('pdf (concat " " (ref-man-export-pdf-template) " "))
                       ('html (concat " " (ref-man-export-html-template) " "))
                       (_ "")))
         (yaml-header (ref-man-export-generate-yaml-header type title tmp-bib-file
                                                           mathjax-path csl-file))
         (cmd "")
         (no-confirm (org-entry-get (point) "NO_CONFIRM"))
         abstract sections bibtexs refs-string)
    (when (and (f-exists? md-file) (not no-confirm))
      (unless (y-or-n-p (format "The file with name %s exists.  Replace? "
                                (f-filename md-file)))
        (user-error "Aborted")))
    (save-restriction
      (save-mark-and-excursion
        (unless buffer
          (org-narrow-to-subtree)
          (apply #'narrow-to-region (ref-man-org-get-bounds-before-references)))
        ;; NOTE: When it's a paper, first paragraph is abstract
        (when (eq type 'paper)
          (goto-char (point-min))
          (ref-man-org-end-of-meta-data)
          (pcase-let ((`(,beg ,end ,has-body) (ref-man-org-text-bounds)))
            (when has-body
              (setq abstract (buffer-substring-no-properties beg end)))
            (narrow-to-region end (point-max))))
        (goto-char (point-min))
        ;; NOTE: If not paper, then collect subtree headings as sections,
        ;;       else buffer headings
        (setq sections (if (eq type 'paper)
                           (util/org-apply-to-buffer-headings
                            (lambda () (concat "*" (org-get-heading t t t t))))
                         (util/org-apply-to-subtree-headings
                          (lambda () (concat "*" (org-get-heading t t t t))) t)))
        ;; Insert standard pandoc references to bibtexs
        ;; NOTE: Generate bibtexs from fuzzy links
        ;; TODO: Raise a `user-error' if error getting the bib from link
        (goto-char (point-min))
        (while (re-search-forward util/org-text-link-re nil t nil)
          ;; NOTE: Don't insert link if it's of an internal section
          (unless (member (match-string 1) sections)
            (let ((bib (ref-man-org-get-bib-from-org-link t t))
                  (title-keys (mapcar (lambda (x) (-take 2 x)) bibtexs)))
              ;; NOTE: Append _a to duplicate bibtex key
              ;; TODO: Fix dups for CUSTOM_ID across org buffer
              (when (-any #'identity (mapcar (lambda (x) (and title-keys
                                                              (string= (cadr x) (cadr bib))
                                                              (not (string= (car x) (car bib)))))
                                             title-keys))
                (setf (cadr bib) (concat (cadr bib) "_a"))
                (setf (nth 2 bib) (replace-regexp-in-string
                                   (string-remove-suffix "_a" (cadr bib))
                                   (cadr bib) (nth 2 bib))))
              (unless (member (cadr bib) (mapcar (lambda (x) (nth 1 x)) bibtexs))
                (push bib bibtexs)))))
        (setq refs-string (ref-man-export-docproc-references bibtexs tmp-bib-file no-gdrive))
        (when refs-string
          (setq yaml-header (concat (string-remove-suffix "---\n" yaml-header) refs-string "---\n")))
        (when abstract
          (setq yaml-header (concat (string-remove-suffix "---\n" yaml-header)
                                    "abstract: >\n" abstract "\n---\n")))
        (when (and (eq type 'paper) ref-man-export-research-article-args)
          (setq yaml-header (concat (string-remove-suffix "---\n" yaml-header)
                                    (yaml-encode ref-man-export-research-article-args)
                                    "\n---\n")))
        (goto-char (point-min))
        ;; NOTE: export to markdown in ref-man-export-md-buffer-name
        (ref-man-export-org-to-md #'util/org-remove-all-time-stamps)
        (with-current-buffer ref-man-export-md-buffer-name
          ;; NOTE: Remove TOC if asked
          (unless with-toc
            (goto-char (point-min))
            (let (toc-min toc-max)
              (re-search-forward "# Table of Contents")
              (beginning-of-line)
              (setq toc-min (point))
              (forward-paragraph 2)
              (setq toc-max (point))
              (delete-region toc-min toc-max)))
          ;; NOTE: Repace "BROKEN LINK"s with bibtexs
          (goto-char (point-min))
          (while (re-search-forward "\\[BROKEN LINK: \\(.+?\\)]" nil t) ; Insert links
            (let ((path (string-remove-prefix "\\" (match-string 1))))
              (replace-match (format "[@%s]" (car (a-get bibtexs path))))))
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
          (write-region (point-min) (point-max) md-file))))
    (unless (eq type 'blog)
      (setq cmd (format "cd %s && %s %s --pandoc-path %s -ro --extra-opts --input-files %s -g %s %s"
                        docproc-dir
                        python
                        pandocwatch
                        ref-man-pandoc-executable
                        md-file
                        (pcase type
                          ('both "html,pdf")
                          ('paper "pdf")
                          (_ type))
                        extra-opts))
      (message "Running command %s" cmd)
      (let ((msg (shell-command-to-string cmd))
            (buf (get-buffer-create "*pandoc and latex output*"))
            case-fold-search)
        (with-current-buffer buf
          (erase-buffer)
          (insert msg)
          (ansi-color-apply-on-region (point-min) (point-max))))
      (when tmp-bib-file
        (delete-file tmp-bib-file)))
    (let ((win (ref-man--get-or-create-window-on-side))
          query-about-changed-file)
      (if out-file
          (unless (string= (buffer-file-name (window-buffer win)) (f-filename out-file))
            (set-window-buffer win (find-file-noselect out-file)))
        (find-file md-file)))))

(provide 'ref-man-export)

;;; ref-man-export.el ends here
