;;; ref-man-py.el --- Module for managing the files. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019,2020,2021,2022
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Wednesday 14 December 2022 11:15:17 AM IST>
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
;; Functions for managing the (mostly PDF) files for `ref-man'.

;;; Code:


(require 'f)
(require 'ref-man-url)

(defcustom ref-man-documents-dir (expand-file-name "~/.ref-man/docs")
  "Directory where the downloaded pdf files will be stored."
  :type 'directory
  :group 'ref-man)

(defcustom ref-man-extra-documents-dirs nil
  "Directories to scan extra pdf files."
  :type 'directory
  :group 'ref-man)

(defun ref-man-files-filename-from-url (url)
  "Generate unique filename from a given URL.
Returns path concatenated with `ref-man-documents-dir'."
  ;; FIXME: `ref-man-url-maybe-unproxy' should not be here
  (let* ((url (ref-man-url-maybe-unproxy url))
         (obj (url-generic-parse-url url))
         ;; (path (car (url-path-and-query obj)))
         (path (cond ((string-match-p "openreview" url)
                      (if (and (string-match-p "/pdf/" url)
                               (string-suffix-p ".pdf" url))
                          (concat "openreview_" (-last-item (split-string url "/")))
                        (concat "openreview_"
                                (nth 1 (split-string
                                        (cdr (url-path-and-query
                                              (url-generic-parse-url url))) "="))
                                "." "pdf")))
                     ((or (string-match-p "arxiv.org/abs/" url) (string-match-p "arxiv.org/pdf/" url))
                      (concat (f-filename (string-remove-suffix ".pdf" (car (url-path-and-query obj)))) ".pdf"))
                     ((string-match-p "springer.com" url)
                      (concat (string-join (last (split-string url "/") 2) "-") ".pdf"))
                     ((string-match-p "aaai.org" url)
                      (concat "aaai_" (string-join (last (split-string url "/") 2) "_") ".pdf"))
                     ((string-match-p "citeseerx.ist.psu.edu" url)
                      (string-match "\\(.+doi=\\)\\(.+?\\)&.+" url)
                      (concat (match-string 2 url) ".pdf"))
                     ((string-match-p "acm.org" url)
                      (cond ((string-match-p "citation.cfm?id=" url)
                             (concat "acm_" (car (split-string (nth 1 (split-string url "?id=")) "&")) ".pdf"))
                            ((string-match-p "doi/pdf" url)
                             (concat "acm_" (mapconcat #'identity (-take-last 2 (split-string url "/")) "_") ".pdf"))
                            (t (concat "acm_"
                                       (mapconcat #'identity
                                                  (-take-last 2
                                                              (split-string
                                                               (-first-item
                                                                (split-string url "?"))
                                                               "/")) "_") ".pdf"))))
                     (t (when (string-match-p "\\.pdf$" (car (url-path-and-query obj)))
                          (car (url-path-and-query obj))))))
         (file (and path (path-join ref-man-documents-dir (f-filename path)))))
    file))

(defun ref-man-files-dirs-non-hidden (path recurse)
  "Get all non-hidden directories from PATH.
Recurse in directories if RECURSE is non-nil."
  (f-directories path (lambda (x)
                        (not (string-match-p "/\\." x)))
                 recurse))

;; (defun ref-man-files-non-hidden (path recurse)
;;   (directory-files-recursively ref-man-extra-documents-dirs "."))

(defun ref-man-files-fast-files-or-dirs (path f-or-d &optional recurse include-hidden)
  "Get all files or dirs or both or everything, recursively from PATH.
Only works where a POSIX \"find\" is available.

Copy of `util/fast-files-or-dirs'.  Custom local package.

F-OR-D can be one of \\='f \\='files \\='d \\='dirs or \\='both.
If anything else, is given, everything is returned, including
symlinks etc.

Optionally if RECURSE is non-nil recurse into the directories.
INCLUDE-HIDDEN includes hidden files and files in hidden
directories if non-nil.  Uses \"find\" shell command. Much faster
than using `directory-files-recursively'"
  (-remove #'string-empty-p (split-string
                             (shell-command-to-string
                              (format "find %s %s %s %s -print0" path
                                      (if include-hidden "" "-not -path '*/\\.*'")
                                      (pcase f-or-d
                                        ((or 'files 'f) "-type f")
                                        ((or 'dirs 'd) "-type d")
                                        ('both "")
                                        (_ ""))
                                      (if recurse "" "-maxdepth 1"))) "\0")))

(defun ref-man-files-non-hidden (path &optional recurse include-hidden)
  "Get all non-hidden files recursively from PATH.
Optionally if RECURSE is non-nil recurse into the directories.
INCLUDE-HIDDEN includes hidden files and files in hidden
directories if non-nil.  Uses \"find\" shell command. Much faster
than using `directory-files-recursively'"
  (ref-man-files-fast-files-or-dirs path 'f recurse include-hidden))

(defun ref-man-files-file-in-other-dirs (fname)
  "Check if filename FNAME exists in any of `ref-man-extra-documents-dirs'.
Returns first path for that filename."
  ;; NOTE: In case I keep a cache
  ;; (setq ref-man--dirs-mod-time nil)
  (let*
      ;; NOTE: In case I keep a cache
      ;; ((mod-times (mapcar (lambda (x)
      ;;     (cons x  (time-to-seconds
      ;;               (file-attribute-modification-time
      ;;                (file-attributes x)))))
      ;;                      ref-man-other-dirs))
      ;;   (meh (-concat something)))
      ((dirs-files (mapcar (lambda (x) (cons x (ref-man-files-non-hidden x t)))
                           (if (listp ref-man-extra-documents-dirs)
                               ref-man-extra-documents-dirs
                             (list ref-man-extra-documents-dirs))))
       (all-files (apply #'-concat (mapcar #'cdr dirs-files))))
                                        ; return only the first element
    (car (-filter (lambda (x) (or (string= (f-filename x) fname)
                                  (string= (f-filename x) (f-base fname))))
                  all-files))))

(defun ref-man-files-copy-if-required-from-extra-to-documents-dir (file)
  "Copy FILE if not in `ref-man-documents-dir'."
  (when file
    (if (or (not (f-absolute-p file))
            (dir-equal-p (f-dirname file) ref-man-documents-dir))
        file
      (copy-file file (path-join ref-man-documents-dir (f-filename file)))
      (path-join ref-man-documents-dir (f-filename file)))))

(defun ref-man-files-check-pdf-file-exists (uri &optional check-extra)
  "Check if a pdf file exists for a given URI in `ref-man-documents-dir'.
If CHECK-EXTRA is given, then also check in additional
directories given by `ref-man-extra-documents-dirs'"
  (let* ((file-name-a (ref-man-files-filename-from-url uri))
         (file-name-b (and file-name-a (if (string-suffix-p ".pdf" file-name-a)
                                           (string-remove-suffix ".pdf" file-name-a)
                                         (concat file-name-a ".pdf"))))
         (file (and file-name-a file-name-b
                    (cond ((file-exists-p file-name-a) file-name-a)
                          ((file-exists-p file-name-b) file-name-b)
                          (check-extra
                           (let ((file-exists
                                  (ref-man-files-file-in-other-dirs (f-filename file-name-a))))
                             (if file-exists
                                 file-exists
                               (ref-man-files-file-in-other-dirs (f-filename file-name-b)))))
                          (t nil)))))
    (and file (ref-man-files-copy-if-required-from-extra-to-documents-dir file))))


(provide 'ref-man-files)

;;; ref-man-files.el ends here
