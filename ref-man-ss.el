;;; ref-man-ss.el --- Semantic Scholar API calls for `ref-man'. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019,2020,2021,2022
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Tuesday 16 August 2022 09:54:00 AM IST>
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
;; Functions for interacting with Semantic Scholar data fetched from Semantic
;; Scholar Graph API. See https://www.semanticscholar.org/product/api
;;
;; These functions use a local intermediary files cache and interact
;; with a `ref-man-py' process, which is a flask (see https://palletsprojects.com/p/flask/)
;; process doing a bunch of things.


(require 'ref-man-py)
(require 'ref-man-util)


(defun ref-man-ss-fetch-paper-details (ssid &optional update-on-disk)
  "Fetch Semantic Scholar data for ID SSID.

The data is cached on the disk and if the data for entry is
already present, the cached entry is fetched.  With optional
argument UPDATE-ON-DISK, force update the data in cache."
  (let* ((idtype-id ssid)
         (opts `(("id_type" . ,(car idtype-id))
                 ("id" . ,(cdr idtype-id))))
         (opts (if update-on-disk
                   (-concat opts '(("force" . "")))
                 opts))
         (ss-data (when idtype-id
                    (message "[ref-man] %s Semantic Scholar Data for %s id: %s"
                             (if update-on-disk "Force fetching" "Fetching")
                             (car idtype-id) (cdr idtype-id))
                    (with-current-buffer
                        (url-retrieve-synchronously
                         (ref-man-py-url "s2_paper" opts) t) ; silent
                      (goto-char (point-min))
                      (forward-paragraph)
                      (json-read)))))
    ss-data))

(defun ref-man-ss-fetch-paper-citations (ssid &optional params filters)
  "Fetch paper citations for SSID.

Optional PARAMS specifies any filters to be added to the
citations.  By default citations are fetched in increments of
100, but that can be changed with PARAMS.

Optional FILTERS are declarative filters that are applied to the
results.

PARAMS can be queried from the service.

Example PARAMS and FILTERS alist for getting 100 citations from
years 2012-2018:

PARAMS: '((count . 100))

FILTERS: '((year . ((min . 2012) (max . 2018))))
"
  (let* ((url (ref-man-py-url (format "s2_citations/%s" ssid) params))
         (buf (if (and filters (cdr filters))
                  (ref-man--post-json-synchronous url filters t)
                (url-retrieve-synchronously url t))))
    (prog1
        (with-current-buffer buf
          (goto-char (point-min))
          (forward-paragraph)
          (json-read))
      (kill-buffer buf))))

(defun ref-man-ss-parse-search-result (result)
  "Parse the RESULT of a Semantic Scholar search.

RESULT should be an alist."
  (let ((retval nil))
    (when-let ((cites (a-get result 'citationStats)))
      (push `("CITATIONCOUNT". ,(number-to-string (a-get cites 'numCitations))) retval)
      (push `("INFLUENTIALCITATIONCOUNT". ,(number-to-string (a-get cites 'numKeyCitations))) retval))
    (push `("PAPERID". ,(a-get result 'id)) retval)
    (push `("ABSTRACT". ,(a-get (a-get result 'paperAbstract) 'text)) retval)
    (push `("DOI". ,(a-get (a-get result 'doiInfo) 'doi)) retval)
    (push `("URL". ,(a-get (a-get result 'primaryPaperLink) 'url)) retval)
    (push `("YEAR". ,(a-get (a-get result 'year) 'text)) retval)
    (push `("VENUE". ,(a-get (a-get result 'venue) 'text)) retval)
    (when-let ((date (a-get result 'pubDate)))
      (push `("MONTH" . ,(capitalize (a-get ref-man--num-to-months
                                            (string-to-number
                                             (nth 1 (split-string date "-"))))))
            retval))
    (seq-do (lambda (x)
              (if (eq (car x) 'name)
                  (push `("JOURNAL". ,(cdr x)) retval)
                (push `(,(upcase (symbol-name (car x))). ,(cdr x)) retval)))
            (a-get result 'journal))
    (push `("AUTHOR". ,(mapconcat (lambda (x)
                                    (ref-man--build-bib-author
                                     (a-get (aref x 0) 'name)))
                                  (a-get result 'authors) " and "))
          retval)
    (push `("TITLE". ,(a-get (a-get result 'title) 'text)) retval)
    (push '("TYPE" . "article") retval)
    (-remove (lambda (x) (or (not (cdr x)) (string-empty-p (cdr x)))) retval)))

(defun ref-man-ss-search (search-string &rest _args)
  "Search for SEARCH-STRING via Graph API on Semantic Scholar."
  (if (string-empty-p search-string)
      (user-error "Empty Search String")
    (let* ((opts `(("q" . ,search-string)))
           (url (ref-man-py-url "s2_search" opts))
           (buf (url-retrieve-synchronously url))
           (result (with-current-buffer buf
                     (goto-char (point-min))
                     (forward-paragraph)
                     (json-read))))
      (if (eq (car result) 'error)
          (user-error (format "Error occurred %s" (a-get result 'error)))
        (a-get result 'data)))))

(defun ref-man-ss-get-results-search-semantic-scholar (search-string &optional args)
  (let* ((opts (if args
                   (-concat `(("q" . ,search-string)) args)
                 `(("q" . ,search-string))))
         (url (ref-man-py-url "semantic_scholar_search" opts))
         (buf (if args (ref-man--post-json-synchronous url args)
                (url-retrieve-synchronously url)))
         (result (with-current-buffer buf
                   (goto-char (point-min))
                   (forward-paragraph)
                   (json-read)))
         (results (if (eq (car result) 'error)
                      (user-error (format "Error occurred %s" (a-get result 'error)))
                    (a-get result 'results))))
    results))

(defun ref-man-ss-graph-search-results-to-ido-prompts (results)
  "Parse the search RESULTS from SS Graph API as `ido' prompts for user insertion."
  (let ((j 1))
    (mapcar (lambda (x)
              (prog1 (format "%d: %s, %s" j
                             (a-get x 'title)
                             (mapconcat
                              (lambda (y) (a-get y 'name)) (a-get x 'authors) ", "))
                (setq j (+ 1 j))))
            results)))

(defun ref-man-ss-search-results-to-ido-prompts (results)
  "Parse the search RESULTS from SS as `ido' prompts for user insertion."
  (let ((j 1))
    (mapcar (lambda (x)
              (prog1 (format "%d: %s, %s" j
                             (a-get (a-get x 'title) 'text)
                             (mapconcat
                              (lambda (y) (a-get (aref y 0) 'name))
                              (a-get x 'authors) ", "))
                (setq j (+ 1 j))))
            results)))


(defun ref-man-ss-search-presentations-to-ido-prompts (results)
  "Parse the search RESULTS from SS as `ido' prompts for user insertion.

In this case the `matchedPresentations' key is extracted."
  (let ((j 1))
    (mapcar (lambda (x)
              (prog1 (format "%d: %s, %s" j
                             (a-get x 'title)
                             (string-join (a-get x 'authors) ", "))
                (setq j (+ 1 j))))
            results)))

(provide 'ref-man-ss)
