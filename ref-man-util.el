;;; ref-man-util.el --- Utility variables and functions for `ref-man'. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019,2020,2021
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Thursday 09 September 2021 01:23:53 AM IST>
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
;; A bunch of utility functions and some common variables for `ref-man'.

;;; Code:

(require 'bibtex)

(defconst ref-man-stop-words
  '("a" "about" "above" "after" "again" "against"
    "all" "an" "and" "any" "are" "as" "at" "because"
    "before" "below" "between" "both" "by" "can" "did"
    "do" "does" "don" "down" "during" "each" "few"
    "for" "from" "further" "had" "has" "have" "having"
    "here" "how" "i" "in" "into" "is" "it" "its"
    "just" "more" "most" "no" "nor" "not" "now"
    "of" "off" "on" "once" "only" "other" "out"
    "over" "own" "same" "should" "so" "some" "such"
    "t" "than" "that" "the" "then" "there" "these"
    "through" "to" "too" "under" "up" "very" "was" "were"
    "what" "when" "where" "which" "who" "why" "will" "with"))

(defvar ref-man-bibtex-ascii-replacement-strings
  '(("í" . "{\\\\'i}")
    ("æ" . "{\\\\ae}")
    ("ć" . "{\\\\'c}")
    ("é" . "{\\\\'e}")
    ("ä" . "{\\\\\"a}")
    ("è" . "{\\\\`e}")
    ("à" . "{\\\\`a}")
    ("á" . "{\\\\'a}")
    ("ø" . "{\\\\o}")
    ("ë" . "{\\\\\"e}")
    ("ü" . "{\\\\\"u}")
    ("ń" . "{\\\\'n}")
    ("ñ" . "{\\\\~n}")
    ("ņ" . "{\\\\c{n}}")
    ("ñ" . "{\\\\~n}")
    ("å" . "{\\\\aa}")
    ("ö" . "{\\\\\"o}")
    ("á" . "{\\\\'a}")
    ("í" . "{\\\\'i}")
    ("ó" . "{\\\\'o}")
    ("ó" . "{\\\\'o}")
    ("ú" . "{\\\\'u}")
    ("ú" . "{\\\\'u}")
    ("ý" . "{\\\\'y}")
    ("š" . "{\\\\v{s}}")
    ("č" . "{\\\\v{c}}")
    ("ř" . "{\\\\v{r}}")
    ("š" . "{\\\\v{s}}")
    ("İ" . "{\\\\.i}")
    ("ğ" . "{\\\\u{g}}")
    ("α" . "$\\\\alpha$")
    ("β" . "$\\\\beta$")
    ("γ" . "$\\\\gamma$")
    ("ɣ" . "$\\\\gamma$")
    ("δ" . "$\\\\delta$")
    ("η" . "$\\\\eta$")
    ("µ" . "$\\\\mu$")
    ("ɛ" . "$\\\\epsilon$")
    ("λ" . "$\\\\lambda$")
    ("π" . "$\\\\pi$")
    ("∞" . "$\\\\infty$")
    ("χ" . "$\\\\chi$")
    ("ç" . "{\\\\c{c}}")
    ("ß" . "{\\\\ss}")
    ("≤" . "$\\\\le$")
    ("≥" . "$\\\\ge$")
    ("<" . "$<$")
    ("θ" . "$\\\\theta$")
    ("μ" . "$\\\\mu$")
    ("→" . "$\\\\rightarrow$")
    ("⇌" . "$\\\\leftrightharpoons$")
    ("×" . "$\\\\times$")
    ("°" . "$\\\\deg$")
    ("ş" . "{\\\\c{s}}")
    ("º" . "degc")
    ("ⅵ" . "\textrm{vi}")
    ("ⅲ" . "\textrm{iii}")
    ("ⅴ" . "\textrm{v}")
    ("Ⅵ" . "\textrm{VI}")
    ("Ⅲ" . "\textrm{III}")
    ("Ⅴ" . "\textrm{V}")
    ("∼" . "\\\\textasciitilde{}")
    ("‑" . "\\\\textemdash{}")
    ("•" . "\\\\textbullet ")
    ("‒" . "\\\\textemdash{}"))
  "Replace non-ascii characters with escaped ones for latex rendering.
The characters here directly borrowed from `org-ref'.
See `org-ref-nonascii-latex-replacements'")

(defvar ref-man-bibtex-non-invertible-ascii-replacements
  '((" " . " ")
    ("…" . "...")
    (" " . " ")
    (" " . " ")
    (" " . " ")
    ("–" . "-")
    ("−" . "-")
    ("‘" . "'")
    ("’" . "'")
    ("”" . "\"")))

(defun ref-man-pairs-to-alist (pairs)
  "Merge cons PAIRS into an alist with first elements as keys.

The head of the list is the associative element.

Example:
    (pairs-to-alist '((a b) (b c d) (a d) (e f)))
     => '((a b d) (b c d) (e f))"
  (when (and (consp pairs) (a-assoc pairs))
    (let (newlist)
      (seq-do (lambda (x)
                (if (a-has-key newlist (car x))
                    (setq newlist (a-update newlist (car x) (lambda (y) (push (cdr x) y))))
                  (push (list (car x) (cdr x)) newlist)))
              pairs)
      newlist)))

(defun url-join (&rest elements)
  "Join ELEMENTS with a single \"/\", like a url."
  (string-join (-remove #'string-empty-p
                        (mapcar (lambda (x)
                                  (string-remove-prefix "/" (string-remove-suffix "/" x)))
                                elements))
               "/"))

(defun path-join (&rest elements)
  "Join ELEMENTS as a path, expects full paths."
  (concat "/" (mapconcat (lambda (x)
                           (string-remove-prefix "/" (string-remove-suffix "/" x)))
                         elements "/")))

(defun dir-equal-p (dir-a dir-b)
  "Return non-nil if full paths for DIR-A and DIR-B are equal.
They need not exist."
  (string= (string-remove-suffix "/" (expand-file-name dir-a))
           (string-remove-suffix "/" (expand-file-name dir-b))))

;; TODO: Document this
(defun max-ind (seq)
  (let* ((max-ind--max-val 0) (max-ind--temp-ind -1) (max-ind--max 0))
    (cl-loop for x in seq
          do
          (progn
            (setq max-ind--temp-ind (+ 1 max-ind--temp-ind))
            (if x (if (> x max-ind--max-val)
                      (progn (setq max-ind--max-val x)
                             (setq max-ind--max max-ind--temp-ind))))))
    max-ind--max))

(defun find-open-port (init)
  "Find the next open port from INIT in case it's being used by another process."
  (cl-loop for port from init to 65531
        when (string-match-p
              "refused" (shell-command-to-string
                         (format "nc -z -v localhost %s" port)))
        return port))

(defun replace-in-string (in what with)
  "Hackey replace in string.
Replace IN string WITH WHAT."
  (replace-regexp-in-string
   (regexp-quote what) with in nil 'literal))
;; (make-obsolete 'replace-in-string 'replace-regexp-in-string "ref-man 0.3.0")

(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
BEG and END are region markers.  See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

;; TODO: Maybe rename this to a more generic name
(defun ref-man--get-or-create-window-on-side ()
  "This is a copy of the function in util.el."
  (let* ((orig-win (selected-window))
         (win (cond ((window-in-direction 'right orig-win)
                     (window-in-direction 'right orig-win))
                    ((window-in-direction 'left orig-win)
                     (window-in-direction 'left orig-win))
                    (t (split-window-horizontally)))))
    win))

;; CHECK: Should we keep non-ascii?
(defun ref-man--remove-punc (x &optional keep-spaces)
  "Return only alphanumeric characters for string X.
With optional KEEP-SPACES non-nil, don't remove the spaces."
  (if keep-spaces
      (replace-regexp-in-string "[^0-9a-z ]" "" x)
    (replace-regexp-in-string "[^0-9a-z]" "" x)))

(defun ref-man-remove-bookmarks-from-pandoc-tex ()
  "Remove bookmarks from tex generated by pandoc.
The strings are of the form
\"{[}\\\\protect\\\\hyperlink{ref-author2020title}{20}{]}\".
They are replaced with [num], e.g., previous one is replaced with
[20]."
  (interactive)
  (save-excursion
    (save-restriction
      (when (region-active-p)
        (narrow-to-region (region-beginning) (region-end))
        (goto-char (point-min)))
      (while (re-search-forward "{\\[}.*?{\\([0-9]+\\)}{\\]}" nil t nil)
        (replace-match "[\\1]")))))

(defun ref-man--trim-whitespace (str &optional remove-quotes)
  "Trim the input string STR.
Remove newlines and multiple spaces with a single one.  With
optional REMOVE-QUOTES remove all quotes \" from the string
also."
  (let ((str (replace-regexp-in-string "[ \t]+" " "
                                       (replace-regexp-in-string "\n" "" (string-trim str)))))
    (if remove-quotes
        (replace-regexp-in-string "\"" "" str)
      str)))

(defun ref-man--trim-and-unquote (str)
  "Trim the input string STR and remove surrounding quotes if present.
Identical to `ref-man--trim-whitespace' but remove quotes also."
  (let ((str (replace-regexp-in-string "[ \t]+" " "
                                       (replace-regexp-in-string "\n" "" (string-trim str)))))
    (replace-regexp-in-string "\\(.*?\\)\"\\(.+?\\)\"\\(.*\\)" "\\1\\2\\3" str)))

(defun ref-man--fix-curly (str)
  "Gets text between parentheses for {STR}."
  (string-remove-suffix "}" (string-remove-prefix "{" str)))

(defun ref-man--bibtex-key-p (item)
  "ITEM is a bibtex key."
  (string= (car item) "=key="))

(defun ref-man--stop-word-p (x)
  "X is a stop word."
  (member x ref-man-stop-words))

(defun ref-man--transcribe (str change-list &optional inverse)
  "Transcribe non-ascii characters in STR to ASCII lookalikes.
Argument CHANGE-LIST is an alist of regexps, `(A . B)' changes
from A to B.

With optional non-nil INVERSE, do the opposite and change B to
A."
  (let* ((content str)
         (change-list (or change-list bibtex-autokey-transcriptions))
         (keys (if inverse (a-vals change-list) (a-keys change-list)))
         (vals (if inverse (a-keys change-list) (a-vals change-list)))
         (alist (-zip keys vals)))
    (when (string-match-p (string-join keys "\\|") str)
        (pcase-dolist (`(,a . ,b) alist)
          (setq content (replace-regexp-in-string a b content t))))
    content))

(defun ref-man-util-regions-contiguous-p (regions)
  "Return t if list of REGIONS are contiguous."
  (let ((flag t)
        temp)
    (seq-do (lambda (x)
              (if (eq (car x) 'end)
                  (push (cdr x) temp)
                (when (and temp (not (= (- (cdr x) (car temp)) 1)))
                  (setq flag nil))))
            regions)
    flag))

(defun ref-man-delete-blank-lines-in-region (&optional beg end no-trailing-newline)
  "Delete all empty lines in region.
Region is either the active region or optional points BEG and
END.

This function is aliased from URL `https://github.com/akshaybadola/emacs-util'."
  (interactive)
  (when current-prefix-arg
    (setq no-trailing-newline t))
  (save-restriction
    (when (and (called-interactively-p 'any) (region-active-p))
      (setq beg (region-beginning)
            end (region-end)))
    (when (and beg end (< beg end))
      (narrow-to-region beg end)
      (delete-trailing-whitespace)
      (goto-char (point-min))
      (while (re-search-forward "^[ ]+\n" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (when (looking-at "\n")
        (delete-char 1))
      (while (re-search-forward "\r?\n+\n" nil t)
        (replace-match "\n"))
      (goto-char (point-max))
      (when (and no-trailing-newline (looking-back "\n" 1))
        (re-search-backward "\r?\n+\n" nil t)
        (replace-match "")))))

(defun ref-man-delete-blank-lines-in-buffer (&optional buf no-trailing-newline)
  "Delete all empty lines in the entire buffer BUF.
When optional BUF is not given, defaults to current buffer.

This function is aliased from URL `https://github.com/akshaybadola/emacs-util'."
  (interactive)
  (unless buf
    (setq buf (current-buffer)))
  (when current-prefix-arg
    (setq no-trailing-newline t))
  (with-current-buffer buf
    (ref-man-delete-blank-lines-in-region (point-min) (point-max) no-trailing-newline)))

(defun ref-man--replace-non-ascii (str &optional inverse)
  "Replace non-ascii characters in STR with escape codes.

Uses `ref-man-bibtex-ascii-replacement-strings' for replacements.
If `org-ref-nonascii-latex-replacements' exists, then the
replacements are a union of both above alists.

With non-nil optional INVERSE, perform the inverse replacement
from the alist."
  (ref-man--transcribe str (or (and (boundp 'org-ref-nonascii-latex-replacements)
                                    (-union (-concat ref-man-bibtex-ascii-replacement-strings
                                                     ref-man-bibtex-non-invertible-ascii-replacements)
                                            org-ref-nonascii-latex-replacements))
                               ref-man-bibtex-ascii-replacement-strings)
                       inverse))

(defun ref-man-not-pdf-files (&optional dir)
  "Return list of files which are not pdf files in DIR.
If DIR is not given it defaults to `ref-man-documents-dir'."
  (-filter (lambda (x) (let ((case-fold-search t)
                             (pdf-str (shell-command-to-string (format "file '%s'" x))))
                         (not (string-match-p "pdf document" pdf-str))))
           (f-files (or dir ref-man-documents-dir))))

(defun ref-man--invert-accents (str)
  "Replace escaped ascii characters in STR with non-ascii characters.

Performs inverse of `ref-man--replace-non-ascii'."
  (ref-man--transcribe str ref-man-bibtex-ascii-replacement-strings t))

(defun ref-man-save-headings-before-pdf-file-open (arg)
  "Add advice to save headings and paths before calling `org-open-at-point'.
Used to insert ORG_FILE back into the file in case an org buffer
is saved in `ref-man-org-store-dir' from `ref-man-get-references'."
  (if (or (and (integerp arg) (< 0 arg)) arg)
      (advice-add #'org-open-at-point :before #'ref-man-save-heading-before-open-advice)
    (advice-remove #'org-open-at-point #'ref-man-save-heading-before-open-advice)))

(defvar ref-man-headings-before-pdf-open nil
  "Saved org headings and paths when a pdf file was opened from an org buffer.")
(defun ref-man-save-heading-before-open-advice (&optional arg)
  "Save org heading and file path if a pdf file is opened from PDF_FILE property.
This function is used as advice to `org-open-at-point' and
optional ARG is passed through to it.  Org heading and path for
the link are added to `ref-man-headings-before-pdf-open'."
  (let* ((heading (substring-no-properties (org-get-heading t t t t)))
         (context (org-element-context))
         (maybe-link (cond ((and (eq (car context) 'headline)
                                 (plist-get (cadr context) :PDF_FILE))
                            (plist-get (cadr context) :PDF_FILE))
                           ((eq (car context) 'node-property)
                            (plist-get (cadr context) :value))
                           ;; NOTE: we don't save if link is in text (for now)
                           ;; ((eq (car context) 'link)
                           ;;  context)
                           (t nil)))
         (path (cond ((and maybe-link (stringp maybe-link))
                      (pcase (with-temp-buffer
	                       (let ((org-inhibit-startup nil))
	                         (insert maybe-link)
	                         (org-mode)
	                         (goto-char (point-min))
	                         (org-element-link-parser)))
                        (`nil (user-error "No valid link in %S" heading))
                        (link (and link (plist-get (cadr link) :path)))))
                     ((and (eq (car maybe-link) 'link)
                           (equal (plist-get (cadr maybe-link) :type) "file"))
                      (plist-get (cadr maybe-link) :path))
                     (t nil))))
    (when path
      (add-to-list 'ref-man-headings-before-pdf-open (list :heading heading :path path)))))

(provide 'ref-man-util)

;;; ref-man-util.el ends here
