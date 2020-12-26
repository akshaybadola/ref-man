;;; ref-man-core.el --- Core Components for `ref-man'. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019,2020
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Wednesday 24 June 2020 09:16:15 AM IST>
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
;; Core components include data structures and functions and commands forx
;; `org', `bibtex', `science-parse' and `python'. A python flask server
;; interface is used as an interface to arxiv, dblp and semanticscholar.  There
;; are some file and pdf functions also and functions specific for gscholar
;; also.
;;
;; Perhaps I'll add functions for markdown also (to convert to manuscript)
;; though `org' export can also be used, though I'll have to reconfigure my
;; settings for that and I'm not sure if it'll be as capable as pandoc.
;;
;; Actually `org-ref' has a lot of useful features and I feel in my zeal I've
;; reinvented the wheel a bit, but some of the features here I didn't have
;; so...not sure.  Especially the `eww' ones.  semanticscholar also I won't find I
;; think easily.
;;
;; TODO: I have to separate these according to:
;;       - ref-man-bibtex
;;       - ref-man-org
;;       - ref-man-pdf
;;       - ref-man-dblp
;;       - ref-man-ss (for semanticscholar)
;;       - ref-man-gscholar (for gscholar utils)
;;       python utils are in any case separate
;;
;; NOTE: I think I'll keep the python interface, the processes, the data
;;       structures all here and move org to a new file next.
;;
;; TODO: Also the code is very messy and very little documentation.  I have to
;;       add them
;;
;; TODO: (if (eq major-mode 'org-mode) ,body (message "not in org mode") nil)
;;       Should be a macro I think or an advice

;;; Code:

(require 'async)
(require 'biblio-core)
(require 'bibtex)   ; Primary function I use from 'bibtex is 'bibtex-parse-entry
(require 'bind-key)
(require 'cl-lib)
(require 'dash)
(require 'dash-functional)
(require 'eww)
(require 'gscholar-bibtex)              ; NOTE: Maybe remove this eventually
(require 'json)
(require 'org)
(require 'org-ref)                      ; loads doi-utils also
(require 'org-element)
(require 'seq)
(require 'shr)
(require 'subr-x)
(require 'thingatpt)
(require 'url)
(require 'xml)

(require 'ref-man-util)
(require 'ref-man-files)
(require 'ref-man-url)
(require 'ref-man-web)

(defgroup ref-man nil
  "Bibliography Manager"
  :prefix "ref-man-"
  :group 'org)

(defcustom ref-man-data-root-dir (expand-file-name "~/.ref-man")
  "Root directory where ref-man data is stored."
  :type 'directory
  :group 'ref-man)

(defcustom ref-man-org-links-file-path (expand-file-name "~/.ref-man/.temp-org-links.org")
  "Temporary org file to hold URLs and metadata."
  :type 'file
  :group 'ref-man)

(defcustom ref-man-temp-bib-file-path (expand-file-name "~/.ref-man/.temp.bib")
  "Temprory bib file to append any extract bibtex info."
  :type 'file
  :group 'ref-man)

(defcustom ref-man-org-store-dir (expand-file-name "~/.ref-man/org/")
  "Directory where the org files corresponding to documents will be stored."
  :type 'directory
  :group 'ref-man)

(defcustom ref-man-update-pdf-url-when-download nil
  "When non-nil insert/update PDF_URL property of heading when fetching pdf."
  :type 'boolean
  :group 'ref-man)

(defcustom ref-man-python-server-port-start 9999
  "Server port on which to communicate with python server."
  :type 'integer
  :group 'ref-man)

(defcustom ref-man-python-server-port 9999
  "Port on which to communicate with python server."
  :type 'integer
  :group 'ref-man)

(defcustom ref-man-python-process-use-venv nil
  "Whether to initialize and use a virtualenv for the python process."
  :type 'boolean
  :group 'ref-man)

(defcustom ref-man-python-data-dir (expand-file-name "~/.ref-man/data/")
  "Server port on which to communicate with python server."
  :type 'directory
  :group 'ref-man)

(defcustom ref-man-proxy-port nil
  "Whether to use http proxy for all python requests.
If this is non-nil then the all the requests by the python server
go through this http proxy at localhost, specified by this port."
  :type 'integer
  :group 'ref-man)

(defvar ref-man-key-list
  '(authors title venue volume number pages year doi ee)
  "Only these keys from bibtex are retained (I think).")

(defvar ref-man-bibtex-save-ring
  nil
  "List to store parsed bibtex entries when they're not killed.")

;; NOTE: External functions
(declare-function ref-man-try-start-science-parse-server "ref-man")
(declare-function ref-man-kill-science-parse-process "ref-man")

(seq-do (lambda (x)
          (when (and x (not (f-exists-p x)))
            (make-directory x)))
        (list ref-man-data-root-dir ref-man-org-store-dir
              ref-man-documents-dir ref-man-extra-documents-dirs
              ref-man-python-data-dir))

;; (setq ref-man-org-links-file-path (expand-file-name "~/.temp-org-links.org"))
;; (setq ref-man-documents-dir (expand-file-name "~/org/pdfs/"))
;; ;; (setq ref-man-temp-bib-file-path (expand-file-name "~/lib/docprocess/all.bib"))
;; (setq ref-man-temp-bib-file-path (expand-file-name "~/.temp.bib"))
;; (setq ref-man-org-store-dir (expand-file-name "~/org/pubs_org/"))

;; Internal global variables
;; FIXME: ref-man--org-gscholar-launch-buffer etc. are still being
;;        used causing confusion
;; (setq ref-man--org-gscholar-launch-buffer nil)
;; (setq ref-man--org-gscholar-launch-point nil)

;; NOTE: External variables
;; from `ref-man'
(defvar ref-man-home-dir)
(defvar ref-man-science-parse-server-port)

;; (declare-function 'string-match-p "subr")

;; NOTE: Internal variables
(defvar ref-man--org-gscholar-launch-buffer nil)
(defvar ref-man--org-gscholar-launch-point nil)
(defvar ref-man--eww-import-link nil)
(defvar ref-man--subtree-list nil)
(defvar ref-man--current-org-buffer nil)
(defvar ref-man--science-parse-data nil)
(defvar ref-man--json-data nil)
(defvar ref-man--document-title nil)
(defvar ref-man--current-pdf-file-name nil)
(defvar ref-man--biblio-callback-buf nil)
(defvar ref-man-external-python-process-pid nil)

(defvar shr-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'shr-show-alt-text)
    (define-key map "i" 'shr-browse-image)
    (define-key map "z" 'shr-zoom-image)
    (define-key map [?\t] 'shr-next-link)
    (define-key map [?\M-\t] 'shr-previous-link)
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] 'shr-browse-url)
    (define-key map "I" 'shr-insert-image)
    (define-key map "w" 'shr-copy-url)
    (define-key map "u" 'shr-copy-url)
    (define-key map "RET" 'shr-browse-url)
    (define-key map "o" 'shr-save-contents)
    (define-key map "\r" 'shr-browse-url)
    map))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START ref-man constants  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Constants. perhaps can name them better
;; Also should be shifted to defcustom
;;
(defvar ref-man-venue-priorities
  (let* ((confs '("icml" "nips" "iccv" "cvpr" "ijcai" "aaai" "eccv"))
         (confs-seq (number-sequence (length confs) 1 -1)))
    (cl-mapcar 'cons confs confs-seq))
  "Venue priority list from high to low.")

(defconst ref-man--num-to-months
  '((1 . "Jan") (2 . "Feb") (3 . "Mar") (4 . "Apr")
    (5 . "May") (6 . "Jun") (7 . "Jul") (8 . "Aug")
    (9 . "Sep") (10 . "Oct") (11. "Nov") (12 . "Dec")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END ref-man constants  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START ref-man string utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: Ugly hack? Maybe change with an alist and transcribe

(defun ref-man-bibtex-transcribe (key)
  "Transcribe non-ascii characters in bibtex KEY to ASCII lookalikes.
Transcription is done using `bibtex-autokey-transcriptions'.  I
think the function is copied from `bibtex'."
  (ref-man--transcribe key bibtex-autokey-transcriptions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END ref-man string utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START Bib entry utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: All these bib functions are a huge mess
(defun ref-man--preferred-venue (results)
  "Return the preferred venue for RESULTS.
Venues are looked up in `ref-man-venue-priorities'.  If multiple
venues are found for a result, pick the one where venue has
higher priority."
  (if (= 1 (length results))
      0
    (let* ((venues (mapcar (lambda (x)
                             (gscholar-bibtex--xml-get-child x 'venue))
                           results))
           (prefs (mapcar (lambda (x)
                            (cdass (downcase (car (last x))) ref-man-venue-priorities))
                          venues)))
      prefs)))

(defun ref-man--preferred-venue-vector (results)
  "Like `ref-man--preferred-venue' but for a vector RESULTS."
  (if (= 1 (length results))
      0
    (let* ((venues (mapcar (lambda (x)
                             (cdr (assoc 'venue x)))
                           results))
           (venues (mapcar (lambda (x)
                             (cond ((vectorp x) (downcase (aref x 0)))
                                   ((stringp x) (downcase x))
                                   (t nil)))
                           venues))
           (prefs (mapcar (lambda (x) (cdr (assoc x ref-man-venue-priorities))) venues)))
      prefs)))

;; CHECK: What does this do exactly?
(defun ref-man--validate-author (author)
  "Remove numbers and stuff from AUTHOR string."
  (if (or (string-match-p "[0-9]+" (car (last author)))
                  (string-match-p "^i$\\|^ii$\\|^iii$\\|^iv$" (downcase (car (last author)))))
      (if (> (length author) 2) (butlast author) (nconc (butlast author) '("")))
    author))


(defun ref-man--dblp-clean-helper (result)
  "Subroutine with xml RESULT for `ref-man-dblp-clean'."
  (remove '("nil")
          (mapcar
           (lambda (x)
             (if (eq x 'authors)
                 (list
                  (symbol-name 'authors)
                  (string-join (mapcar (lambda (x) (car (last x)))
                                       (-drop 2 (gscholar-bibtex--xml-get-child result x))) ", "))
               (cons (symbol-name (car (gscholar-bibtex--xml-get-child result x)))
                     (last (gscholar-bibtex--xml-get-child result x)))))
           ref-man-key-list)))

;;
;; clean the xml entry and keep relevant itmes. uses gscholar-bibtex
;;
(defun ref-man-dblp-clean (results &optional all)
  "Clean the xml entry and keep relevant itmes according to `ref-man-key-list'.
RESULTS are results obtained from parsing xml from dblp.
Optional ALL specifies to process all results.  By default only
the top result is processed.

Uses `gscholar-bibtex'.  If ALL is NIL returns only the top
processed result according to `ref-man-venue-priorities'"
  (if (and results all)
      (mapcar #'ref-man--dblp-clean-helper results)
    (let ((result (nth (max-ind (ref-man--preferred-venue results)) results)))
      (when result
        (ref-man--dblp-clean-helper result)))))

;; NOTE: Only used by `ref-man--dblp-fetch-python-process-results'
(defun ref-man--dblp-clean-vector (result)
  "Clean the xml entry and keep relevant itmes according to `ref-man-key-list'.

Uses `gscholar-bibtex'.  Returns an alist with symbol keys for
only the top RESULT from `ref-man-venue-priorities'"
  ;; FIXME: inds is not used
  (let* ((inds (ref-man--preferred-venue-vector result))
         (result (aref result (max-ind (ref-man--preferred-venue-vector result)))))
    ;; TODO: handle this later
    (if result
        (remove '("nil")
                (mapcar
                 (lambda (x)
                   (if (eq x 'authors)
                       (list
                        (symbol-name 'authors)
                        ;; NOTE: There was a bug that messed up the names because I was building "bib-author" twice
                        ;; (mapconcat (lambda (x) (let ((splits (split-string x)))
                        ;;                          (concat (car (last splits))
                        ;;                                  ", " (string-join (butlast splits) " "))))
                        ;;            (cdr (assoc x result)) " and ")
                        (mapconcat #'identity (cdr (assoc x result)) ", "))
                     (list (symbol-name x) (cdr (assoc x result)))))
                 ref-man-key-list)))))

(defun ref-man--build-bib-key-from-plist (str-plist)
  "Builds a unique key with the format [author year first-title-word].
Entry STR-PLIST is a plist."
  (let* ((first-author-str (car (split-string (ref-man--trim-and-unquote
                                               (plist-get str-plist :author)) ",")))
         (first-author (ref-man--validate-author (split-string first-author-str " " t)))
         (last-name (car (last first-author)))
         (year-pub (ref-man--trim-and-unquote (plist-get str-plist :year)))
         (title (-remove 'ref-man--stop-word-p
                           (mapcar #'ref-man--remove-punc
                                   (split-string (downcase (ref-man--trim-and-unquote
                                                            (plist-get str-plist :title))) " "))))
         (title-first (car (split-string (car title) "-")))
         (key (ref-man--replace-non-ascii (mapconcat 'downcase (list last-name year-pub title-first) "")))
         (key (ref-man-bibtex-transcribe (ref-man--remove-punc key))))
    key))

;;
;; TODO: crossref and dblp insert URL as dx.doi.org something which
;; redirects to the real url or may not even in some cases. If a pdf
;; url exists, don't mess with it and insert it as doi.
;;
;;
(defun ref-man--build-bib-key (key-str &optional na)
  "Builds a unique key with the format [author year first-title-word].
Entry KEY-STR is an alist of string keys.  Optional NA argument
appends \"na_\" if the key is non-authoritative."
  (let* ((first-author-str (car (split-string (ref-man--trim-and-unquote (cadr (assoc "authors" key-str))) ",")))
         (first-author (ref-man--validate-author (split-string first-author-str " " t)))
         (last-name (car (last first-author)))
         (year-pub (ref-man--trim-and-unquote (car (cdr (assoc "year" key-str)))))
         (title (-remove 'ref-man--stop-word-p
                           (mapcar #'ref-man--remove-punc
                                   (split-string (downcase (ref-man--trim-and-unquote
                                                            (cadr (assoc "title" key-str)))) " "))))
         (title-first (car (split-string (car title) "-")))
         (key (ref-man--replace-non-ascii (mapconcat 'downcase (list last-name year-pub title-first) "")))
         (key (ref-man-bibtex-transcribe (ref-man--remove-punc key))))
    (if na (concat "na_" key) key)))

(defun ref-man--build-bib-key-from-parsed-org-bibtex (bib-alist)
  "Builds a unique key with the format [author year first-title-word].
BIB-ALIST is an plist of parsed bibtex entry.  Returns the
trimmed entries and converts multiple spaces to a single one."
  (let* ((first-author-str (car (split-string (ref-man--trim-and-unquote (cdr (assoc :author bib-alist))) ",")))
         (first-author (ref-man--validate-author (split-string first-author-str " " t)))
         (last-name (car (last first-author)))
         (year-pub (ref-man--trim-and-unquote (cdr (assoc :year bib-alist))))
         (title (-remove 'ref-man--stop-word-p
                           (mapcar #'ref-man--remove-punc
                                   (split-string (downcase (ref-man--trim-and-unquote
                                                            (cdr (assoc :title bib-alist)))) " "))))
         (title-first (car (split-string (car title) "-")))
         (key (ref-man--replace-non-ascii (mapconcat 'downcase (list last-name year-pub title-first) "")))
         (key (ref-man-bibtex-transcribe (ref-man--remove-punc key))))
    key))

(defun ref-man--build-bib-key-from-parsed-bibtex (bib-alist)
  "Builds a unique key with the format [author year first-title-word].
BIB-ALIST is an alist of string keys.  Assumes the strings are all validated"
  (let* ((last-name (car (split-string
                          (car (split-string (ref-man--fix-curly
                                              (cdr (assoc "author" bib-alist))) " and ")) ", ")))
         (year-pub (cdr (assoc "year" bib-alist)))
         (title (-remove 'ref-man--stop-word-p
                         (split-string (ref-man--fix-curly
                                        (downcase (cdr (assoc "title" bib-alist)))) " ")))
         (title-first (car (split-string (car title) "-"))))
    (ref-man-bibtex-transcribe
     (ref-man--remove-punc
      (ref-man--replace-non-ascii
       (mapconcat 'downcase (list last-name year-pub title-first) ""))))))

;; TODO: Rename this
;; CHECK: It's not even used anywhere
(defun ref-man--build-bib-assoc-from-parsed-org-bibtex (bib-alist)
  "Builds the str alist of bib from symbol BIB-ALIST.
Can be used to build both the bib entry and org entry."
  (let* ((key (ref-man--build-bib-key-from-parsed-org-bibtex bib-alist))
         (author (cons "author" (ref-man--trim-and-unquote (cdr (assoc :author bib-alist)))))
         (title (cons "title" (ref-man--trim-and-unquote (cdr (assoc :title bib-alist)))))
         (year (cons "year" (ref-man--trim-and-unquote (cdr (assoc :year bib-alist)))))
         (doi (cons "doi" (cdr (assoc :doi bib-alist))))
         (volume (cons "volume"  (cdr (assoc :volume bib-alist))))
         (number (cons "number"  (cdr (assoc :number bib-alist))))
         (pages  (cons "pages" (cdr (assoc :pages bib-alist))))
         (publisher  (cons "publisher" (cdr (assoc :publisher bib-alist))))
         (abstract (cons "abstract" (cdr (assoc :abstract bib-alist))))
         (url (cons "url" (cdr (assoc :ee bib-alist))))
         (url (if url url (cons "url" (cdr (assoc :url bib-alist)))))
         (tmp-venue (cdr (assoc :journal bib-alist))) ;; TODO: expand venue
         (tmp-venue (if tmp-venue tmp-venue (cdr (assoc :booktitle bib-alist)))) ;; TODO: expand venue
         (tmp-venue (if tmp-venue tmp-venue (cdr (assoc :venue bib-alist)))) ;; TODO: expand venue
         (venue (cons "venue" tmp-venue)) ;; TODO: expand venue
         (howpublished (cdr (assoc :howpublished bib-alist)))
         (howpublished (when (and howpublished (> 1 (length (split-string howpublished "{"))))
                         (when (string-match-p "url" (nth 0 (split-string howpublished "{")))
                           (car (split-string (nth 1 (split-string howpublished "{")) "}"))))))
    (list key (-filter 'cdr (list abstract author title year doi
                                  volume number pages url venue publisher howpublished)))))

(defun ref-man--build-bib-author (author-str)
  "Return bibtex format author from string AUTHOR-STR.

For example, for an input \"Samy Bengio and Oriol Vinyals and
Navdeep Jaitly and Noam Shazee\", it'll split at \"and\" and
transpose the last name as the first element of each name
inserting a comma there, resulting in \"Bengio, Samy and Vinyals,
Oriol and Jaitly, Navdeep and Shazee, Noam\"."
  (let* ((author-str (ref-man--replace-non-ascii author-str))
         (author-str (replace-in-string (replace-in-string author-str "\\.$" "") ",$" ""))
         (authors (split-string author-str "," t))
         (result-authors
          (mapcar (lambda (x)
                    (let ((temp-auth (ref-man--validate-author (split-string x " " t))))
                      (if (= 1 (length temp-auth)) (car temp-auth)
                        (mapconcat 'identity (list (car (last temp-auth))
                                                   (mapconcat 'identity
                                                              (butlast temp-auth) " ")) ", ")))) authors)))
    (mapconcat 'identity result-authors " and ")))

(defun ref-man--build-vernacular-author (author-str)
  "Builds  common spoken English author from AUTHOR-STR.
Assumes that the input is in bib_author format.

For example, for an input \"Bengio, Samy and Vinyals, Oriol and
Jaitly, Navdeep and Shazee, Noam\", it'll split at \"and\" and
transpose the last names to the proper place, resulting in \"Samy
Bengio and Oriol Vinyals and Navdeep Jaitly and Noam Shazee\"."
  (let* ((author-str (replace-in-string (replace-in-string author-str "\\.$" "") ",$" ""))
         (authors (split-string author-str " and " t "[ ]+"))
         (result-authors
          (mapcar (lambda (x) (mapconcat 'identity (reverse (split-string x ", ")) " "))
                  authors))
         (result-authors (mapconcat 'identity result-authors " and ")))
    result-authors))

(defun ref-man--build-bib-assoc (key-str &optional na)
  "Return a list of string cons'es from DBLP entry KEY-STR.

The car of the list is a bibtex key generated by
`ref-man--build-bib-key'.  With non-nil NA, prefix the generated
bibtex key with \"na_\"."
  (let* ((key (ref-man--build-bib-key key-str na))
         (author (cons "author" (ref-man--build-bib-author
                                 (cadr (assoc "authors" key-str)))))
         (title (cons "title" (cadr (assoc "title" key-str))))
         (year (cons "year" (cadr (assoc "year" key-str))))
         (doi (cons "doi" (cadr (assoc "doi" key-str))))
         (volume (cons "volume" (cadr (assoc "volume" key-str))))
         (number (cons "number" (cadr (assoc "number" key-str))))
         (tmp-pages (cadr (assoc "pages" key-str)))
         (pages (cons "pages" (when tmp-pages
                                (replace-in-string
                                 (replace-in-string tmp-pages "-" "--") " " ""))))
         (url (cons "url" (cadr (assoc "ee" key-str))))
         (venue (cons "venue" (cadr (assoc "venue" key-str)))))
    (list key (-filter 'cdr (list author title year doi volume number pages url venue)))))
(make-obsolete 'ref-man--build-bib-assoc nil "ref-man 0.3.0")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END Bib entry utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START Org generation and insertion stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: What if science-parse is called on multiple pdf files?
;;       For that I'll have to hold all the data in a list
;; TODO: Add async callback with message "DONE" when done
(defun ref-man-get-references ()
  "Extract references for the current pdf document.

Uses Science Parse server to extract the references and DBLP to
verify the bibliography entries.  The results are extracted to an
org buffer generated by `ref-man--generate-buffer-and-fetch-if-required'.

Each such org buffer is given a unique name which is the same as
the bibtex key for that publication.  In case the file already
exists on disk in `ref-man-org-store-dir', that is opened instead
of fetching from DBLP again.

For entries which aren't identified by DBLP, an \"na_\" is
prefixed to the name where \"na\" stands for \"non authoritative\".
For example for a publication if the bibtex key
would be \"name1995something\" but DBLP can't find it in the
database, the bibtex key (and file) becomes
\"na_name1995something\"(.org)."
  (interactive)
  ;; (setq my/dblp-results nil)            ; TODO: Not used after change to python backend
  (let ((status (ref-man-try-start-science-parse-server)))
    (cond ((not status)
           (ref-man-try-start-science-parse-server))
          ((eq status 'waiting)
              (message (message "[ref-man] Waiting for Science Parse server to become ready.")))
          (t
           (let*
               ((pdf-file-name (expand-file-name (buffer-file-name (current-buffer))))
                (json-string
                 (if (string-equal major-mode "pdf-view-mode")
                     (shell-command-to-string (format "curl -s -H\
            \"content-type: application/pdf\" --data-binary @%s\
            \"http://localhost:%s/v1\"" pdf-file-name ref-man-science-parse-server-port))
                   (progn (message "[ref-man] not pdf-view-mode") nil)))
                ;; NOTE: In case socks proxy with proxychains is used
                (json-string (replace-in-string json-string "[proxychains] DLL init: proxychains-ng 4.13\n[proxychains] DLL init: proxychains-ng 4.13\n[proxychains] config file found: /home/joe/.proxychains.conf\n[proxychains] preloading /usr/lib64/proxychains-ng/libproxychains4.so\n[proxychains] DLL init: proxychains-ng 4.13\n" ""))
                (json-object-type 'hash-table)
                (json-key-type 'string)
                (json-array-type 'list)
                (json-string (if json-string (json-read-from-string json-string) nil))
                ;; concats title and authors for each ref entry for easy lookup
                (refs-list (if json-string
                               (mapcar (lambda (x)
                                         (cons (concat (gethash "title" x) " "
                                                       (string-join (gethash "authors" x) " "))
                                               x)) (gethash "references" json-string))
                             nil)))
             (if json-string
                 (progn
                   (setq ref-man--current-pdf-file-name pdf-file-name)
                   (setq ref-man--science-parse-data json-string)
                   (setq ref-man--document-title (if (gethash "title" json-string)
                                                     (gethash "title" json-string)
                                                   (puthash "title" (read-from-minibuffer
                                                                     "ENTER TITLE (could not infer): ")
                                                            json-string)
                                                   (gethash "title" json-string)))
                   (ref-man--generate-buffer-and-fetch-if-required refs-list))
               (progn (message "[ref-man] Empty PDF parse") nil)))))))

(defun ref-man--create-org-buffer (&optional visiting-filename)
  "Create an org buffer where all the fetch results will be inserted.
With optional VISITING-FILENAME the buffer name and file are set
to VISITING-FILENAME instead of `ref-man--document-title'."
  (let ((buf (get-buffer-create
              (if visiting-filename visiting-filename
                (concat ref-man--document-title "_org"))))
        (win (ref-man--get-or-create-window-on-side)))
    (set-window-buffer win buf)
    (with-current-buffer buf (org-mode)) buf))

(defun ref-man--generate-org-buffer-content (org-buf refs-list bib-assoc visiting-filename)
  "Populate ORG-BUF for a BIB-ASSOC with entries from REFS-LIST.
BIB-ASSOC is used to generate the top level heading corresponding
to the publication.  REFS-LIST are similar alists which are
fetched from dblp with `ref-man--dblp-fetch-python' and synced
before calling this function.  VISITING-FILENAME is the filename
for the buffer."
  (with-current-buffer org-buf
    (ref-man--org-bibtex-write-top-heading-from-assoc bib-assoc)
    (org-insert-heading-after-current)
    (org-demote-subtree)
    (insert "Refs")
    (org-insert-heading-after-current)
    (org-demote-subtree)
    (end-of-line)
    (message "[ref-man] Fetching references from DBLP")
    ;; TODO: Maybe set buffer read only until fetched?
    (ref-man--dblp-fetch-python refs-list org-buf)
    (set-visited-file-name visiting-filename)))

;; CHECK: Do I really need to send URL here? Maybe for debugging but there's no
;;        debugging here. status of course is sent automatically
;;
;; CHECK: This should be named differently perhaps.
(defun ref-man--post-json-callback (status url callback)
  "Callback to parse a response buffer for HTTP POST call with JSON data.
STATUS is HTTP status, URL the called url, and CALLBACK is the
callback which will be called after parsing the JSON data."
  (goto-char (point-min))
  (forward-paragraph)
  (setq ref-man--json-data (json-read))
  (apply callback (list ref-man--json-data)))

(defun ref-man--post-json-synchronous (url data)
  "Send an HTTP POST request to URL with DATA.
DATA should be an alist of key-value pairs.  The request is sent
content-type as application/json and DATA is encoded as json."
  (let ((url-request-extra-headers
         `(("Content-Type" . "application/json")))
        (url-request-method "POST")
        (url-request-data
         (encode-coding-string (json-encode-alist data) 'utf-8)))
    (url-retrieve-synchronously url)))

(defun ref-man--post-json (url queries callback)
  "Send an HTTP POST with JSON data request to URL.
QUERIES is a list of strings which is encoded as json.  The
request is sent with content-type as application/json.

CALLBACK is passed as an argument to
`ref-man--post-json-callback' after the URL is retrieved.
`ref-man--post-json-callback' decodes the JSON data to elisp
structures and then calls CALLBACK on it."
  (let ((url-request-extra-headers
         `(("Content-Type" . "application/json")))
        (url-request-method "POST")
        (url-request-data
         (encode-coding-string (json-encode-list queries) 'utf-8)))
    (url-retrieve url #'ref-man--post-json-callback
                  (list url callback))))

(defun ref-man--post-json-new (url encode-func args callback)
  "Send an HTTP POST request with JSON data to URL.
More general than `ref-man--post-json'.  The data and parameters
are given to ENCODE-FUNC as ARGS and can have any type.  The
output must be a string.  CALLBACK is used in a similar way to
`ref-man--post-json'."
  (let ((url-request-extra-headers
         `(("Content-Type" . "application/json")))
        (url-request-method "POST")
        (url-request-data
         (encode-coding-string (funcall encode-func args) 'utf-8)))
    (url-retrieve url #'ref-man--post-json-callback
                  (list url callback))))

(defun ref-man--dblp-fetch-python-process-results (refs-list org-buf results)
  "Utility function to process results from the python server.

It's passed as an argument to `ref-man--post-json' but as a
partial application with list of queries REFS-LIST and target
buffer ORG-BUF fixed.  `ref-man--post-json-callback' processes
the HTTP response buffer, converts JSON data to elisp and then
calls the partial function with sole argument RESULTS."
  ;; NOTE: Sometimes result is hash-table and sometime alist
  (let ((na-results (cond ((listp results)
                           (-filter (lambda (x) x)
                                    (mapcar (lambda (x)
                                              (when (and (vectorp (cdr x))
                                                         (stringp (aref (cdr x) 0))
                                                         (string= (aref (cdr x) 0) "NO_RESULT"))
                                                (prog1 (format "%s" (car x))
                                                  (delq x results))))
                                            results)))
                          ((hash-table-p results)
                           (-filter (lambda (x) x)
                                    (mapcar (lambda (x)
                                              (when (and (stringp (car (gethash x ref-man--json-data)))
                                                         (string= (car (gethash x ref-man--json-data)) "NO_RESULT"))
                                                (prog1 x
                                                  (remhash x results))))
                                            (hash-table-keys results)))))))
    ;; NOTE: First write headings which are retrieved
    (seq-do (lambda (x)
              (if (and (vectorp (cdr x))
                       (stringp (aref (cdr x) 0)))
                  (add-to-list 'na-results (format "%s" (car x)))
                (with-current-buffer org-buf
                  (ref-man--org-bibtex-write-ref-from-assoc
                   (ref-man--build-bib-assoc (ref-man--dblp-clean-vector (cdr x)))))))
            results)
    ;; NOTE: Then non authoritative headings
    (seq-do (lambda (x)
              (with-current-buffer org-buf
                (ref-man--org-bibtex-write-ref-NA-from-keyhash
                 (cdr (assoc x refs-list)))))
            na-results)
    (with-current-buffer org-buf
      (outline-up-heading 1)
      (forward-line)
      (kill-line)
      (delete-blank-lines)
      (save-buffer))
    (message (format "Inserted %s references from DBLP, %s from SP"
                     (- (length refs-list) (length na-results)) (length na-results)))))

;; TODO: Need another function to fetch a search string at prompt and copy to kill ring
(defun ref-man--dblp-fetch-python (refs-list org-buf)
  "Fetch publication queries in parallel from DBLP.
Uses a python server which parallelizes the queries and sends
result.  The queries are `car's of REFS-LIST.  ORG-BUF is the
target buffer where the results are inserted.

There was an implementation with `async' initially, but the
buffer would hang while at the sync step waiting for the result
and the entire process was very messy.  Parallel network calls in
python are much easier and cleaner."
  ;; (setq ref-man--temp-ref nil)
  (let ((queries (mapcar 'car refs-list))
        (url (format "http://localhost:%s/dblp" ref-man-python-server-port)))
    ;; ;; NOTE: For `ref-man--post-json-new' encode-func has to be provided
    ;; ;;       Not using for now
    ;; (encode-func json-encode-list))
    ;; (ref-man--post-json-new url encode-func queries
    ;;                     (-cut ref-man--dblp-fetch-python-process-results refs-list org-buf <>))
    ;; NOTE: partial function which will process results eventually
    (ref-man--post-json url queries
                        (-cut ref-man--dblp-fetch-python-process-results refs-list org-buf <>))))

;;
;; Called by ref-man--generate-buffer-and-fetch-if-required
;; NOTE: I was thinking to make it async but it's still useful
;;
(defun ref-man-dblp-fetch-serial (query &optional all)
  "Fetch the dblp data synchronously for query.
QUERY should be the title string of pubilcation or a combination
of title or author string.  When called from
`ref-man--generate-buffer-and-fetch-if-required', it's used to
insert the top level heading.

By default returns only the top result.  With non-nil ALL, returns
all results."
  (message "[ref-man] Fetching from DBLP synchronously.")
  (let* ((query (replace-in-string query " " "+"))
         (query-url (format "https://dblp.uni-trier.de/search/publ/api?q=%s&format=xml" query))
         (buf (url-retrieve-synchronously query-url)))
    (with-current-buffer buf (set-buffer-multibyte t))
    (pcase-let ((`(,(and result `(result . ,_)))
                 (xml-parse-region nil nil buf)))
      (remove nil (ref-man-dblp-clean
                   (mapcar (lambda (hit)
                             (gscholar-bibtex--xml-get-child hit 'info))
                           (xml-get-children (gscholar-bibtex--xml-get-child result 'hits) 'hit))
                   all)))))

;; NOTE: Changed add-to-list to push
;; TODO: Change to 
(defun ref-man--generate-key-str-from-science-parse ()
  "Generate a string alist from Science Parse data.
Science Parse data is a hashtable, which is cleaned and the alist
returned."
  (let ((key-str nil))
         (when (gethash "authors" ref-man--science-parse-data)
           (push (cons "authors" (list (mapconcat (lambda (x) (gethash "name" x))
                                                  (gethash "authors" ref-man--science-parse-data) ", ")))
                 key-str))
         (when (gethash "year" ref-man--science-parse-data)
           (push (cons "year" (list (format "%s"
                                            (gethash "year" ref-man--science-parse-data))))
                 key-str))
         (when (gethash "title" ref-man--science-parse-data)
           (push (cons "title"  (list (gethash "title" ref-man--science-parse-data))) key-str))
         (when (gethash "venue" ref-man--science-parse-data)
           (push (cons "venue" (list (gethash "venue" ref-man--science-parse-data))) key-str))
        key-str))
(make-obsolete 'ref-man--generate-key-str-from-science-parse nil "ref-man 0.3.0")

(defun ref-man--generate-buffer-and-fetch-if-required (refs-list)
  "Generate the Org buffer with publication details and references.
REFS-LIST is the list of references to fetch and insert into the
Org buffer.

The generation process is complicated.  Science Parse data is
parsed and the publication title and author are queried from DBLP
to get authoritative information.  A bibtex key is generated by
`ref-man-dblp-fetch-serial' which will be unique for all
publications and the Org buffer filename is the same as that key
+ \".org\".

If the filename exists in `ref-man-org-store-dir'; implying that
the queries were sent before some time; that file is opened
instead.  If it doesn't exist, then REFS-LIST is sent to a python
server which parallelizes and syncs the results from DBLP.  The
results are then formatted as org entries and inserted into the
buffer with that filename."
  (let* ((query (concat
                    (replace-regexp-in-string "[^\t\n\r\f -~]" ""
                                              (gethash "title" ref-man--science-parse-data)) " "
                    (string-join (mapcar (lambda (x) (gethash "name" x))
                                         (gethash "authors" ref-man--science-parse-data)) " ")))
         (result (ref-man-dblp-fetch-serial query))
         (na (not result))
         (result (or result (ref-man--generate-key-str-from-science-parse)))
         (entry-alist (ref-man--build-bib-assoc result na))
         (filename (car entry-alist))
         (visiting-filename
          (path-join ref-man-org-store-dir (concat (string-remove-prefix "na_" filename) ".org")))
         (buf (find-buffer-visiting visiting-filename)))
    (if (not filename)
        (message "[ref-man] filename could not be generated!")
      (setq filename (string-remove-prefix "na_" filename)) ; always remove na_ from filename
      (cond ((and buf (with-current-buffer buf (buffer-string)))
             (message "[ref-man] File is already opened and not empty. Switching")
             (ref-man--create-org-buffer (concat filename ".org")))
            ((and buf (not (with-current-buffer buf (buffer-string)))
                  (file-exists-p visiting-filename))
             (with-current-buffer (get-buffer-create (concat filename ".org"))
               (insert-file-contents visiting-filename t)))
            ((and (not buf) (file-exists-p visiting-filename))
             (message "[ref-man] File already exists. Opening")
             (let ((org-buf (ref-man--create-org-buffer (concat filename ".org"))))
               (unless (with-current-buffer org-buf
                         (insert-file-contents visiting-filename t) (buffer-string))
                 (ref-man--generate-org-buffer-content org-buf refs-list entry-alist visiting-filename))))
            ((and (not buf) (not (file-exists-p visiting-filename)))
             (let ((org-buf (ref-man--create-org-buffer (concat filename ".org"))))
               (ref-man--generate-org-buffer-content org-buf refs-list entry-alist visiting-filename)))))))

(defun ref-man-org-insert-abstract (abs &optional buf)
  "Insert abstract as text in entry after property drawer if it exists.
ABS is the abstract string.  Insert to `current-buffer' if BUF is
nil else to BUF."
  (unless buf (setq buf (current-buffer)))
  (with-current-buffer buf
    (let ((pblock (org-get-property-block)))
      (when pblock
        (goto-char (cdr pblock))
        (end-of-line)))
    (let ((beg (point))
          (end (progn
                 (outline-next-heading)
                 (point))))
      (delete-region beg (- end 1))
      (goto-char beg))
    (insert "\n")
    (org-indent-line)
    (insert abs)
    (fill-paragraph)))

(defun ref-man--org-bibtex-write-top-heading-from-assoc (entry)
  "Generate the top level org entry for the results org buffer.
ENTRY is bibtex in alist format fetched from DBLP."
  (let* ((key (car entry))
         (entry (nth 1 entry)))
    (org-insert-heading)
    (insert (cdr (assoc "title" entry)))
    (insert "\n")
    (org-indent-line)
    (if (gethash "abstractText" ref-man--science-parse-data)
        (insert (gethash "abstractText" ref-man--science-parse-data))
      (insert "No abstract found")) ;; Hack to get abstractText
    (fill-paragraph)
    (org-insert-property-drawer)
    (cl-loop for ent in entry
          do
          (if (not (string-equal (car ent) "abstract"))
              (org-set-property (upcase (car ent)) (ref-man--fix-curly (cdr ent)))))
    (org-set-property "CUSTOM_ID" key)
    (org-set-property "BTYPE" "article")
    (org-set-property "PDF_FILE" (concat "[[" ref-man--current-pdf-file-name "]]"))))

(defun ref-man--generate-entry-from-hash (hash &optional na)
  "Generate an entry alist from hashtable HASH.
With optional non-nil NA, they \"key\" is prefixed with
\"na_\". It happens in case DBLP or another source cannot resolve
a reference, it's inserted as is prefixed with \"na_\"."
  ;; Need at least author and title
  (when (and (gethash "title" hash) (gethash "authors" hash))
    (let* ((author (cons "author" (ref-man--build-bib-author
                                   (string-join (gethash "authors" hash) ", "))))
           (title (cons "title" (gethash "title" hash)))
           (volume (cons "volume" (gethash "volume" hash)))
           (number (cons "number" (gethash "number" hash)))
           (tmp-pages (cons "pages" (gethash "pages" hash)))
           (pages (cons "pages" (when tmp-pages
                                  (replace-in-string
                                   (replace-in-string (format "%s" tmp-pages) "-" "--") " " ""))))
           (year (cons "year" (when (gethash "year" hash) (format "%s" (gethash "year" hash)))))
           (month (cons "month" (when (gethash "month" hash) (format "%s" (gethash "month" hash)))))
           (venue (cons "venue" (when (gethash "venue" hash)
                                  (replace-in-string (gethash "venue" hash) ",$" ""))))
           ;; (key (mapconcat (lambda (x) (replace-in-string (downcase x) " " ""))
           ;;                 (list "na" "_"
           ;;                       (let ((first-author (split-string (car (gethash "authors" hash)) " ")))
           ;;                         (if (= 1 (length first-author)) (car first-author)
           ;;                           (nth 1 first-author)))
           ;;                       (if (gethash "year" hash) (format "%s" (gethash "year" hash)) "_")
           ;;                       (car (split-string (gethash "title" hash) " " t))) ""))
           (key (ref-man--build-bib-key-from-plist (list :title title :year year :author author)))
           (entry (list key (-filter 'cdr (list author title year month venue volume number pages)))))
      entry)))

(defun ref-man--org-bibtex-write-ref-NA-from-keyhash (key-hash)
  "Write the non authoritative entry to org buffer.
KEY-HASH is the entry from Science Parse data.
`ref-man--generate-entry-from-hash' generates an alist which is written
to the org buffer by `ref-man--org-bibtex-write-ref-from-assoc'."
  (ref-man--org-bibtex-write-ref-from-assoc (ref-man--generate-entry-from-hash key-hash t)))

;; CHECK: Why's entry plist here?
(defun ref-man--org-bibtex-write-ref-from-assoc-misc (entry)
  "Write an @misc bibtex entry from a plist ENTRY."
    (org-insert-heading-after-current)
    (insert (cdr (assoc :title entry)))
    (insert "\n")
    (org-insert-property-drawer)
    (cl-loop for ent in entry
          do
          (when (not (string-equal (symbol-name (car ent)) ":type"))
            (org-set-property (upcase (car (cdr (split-string (symbol-name (car ent)) ":"))))
                              (cdr ent)))))

;; CHECK: This is unused?
(defun ref-man--org-bibtex-write-ref-from-assoc-permissive (entry &optional ignore-errors)
  "Write an org entry from an alist ENTRY parsed from json.
Optional non-nil IGNORE-ERRORS is unused to conform with all the
org writing functions."
  (let* ((key (car entry))
         (entry (nth 1 entry)))
    (org-insert-heading-after-current)
    (insert (cdr (assoc "title" entry)))
    (insert "\n")
    (when (assoc "author" entry)
      (org-indent-line)
      (insert (format "- Authors: %s\n" (ref-man--replace-non-ascii (cdr (assoc "author" entry))))))
    (when (and (assoc "venue" entry) (assoc "year" entry))
      (org-indent-line)
      (insert (format "- %s\n" (concat (cdr (assoc "venue" entry)) ", " (cdr (assoc "year" entry))))))
    (when (assoc "howpublished" entry)
      (org-indent-line)
      (insert (format "- %s\n" (concat "Published as: " (cdr (assoc "venue" entry))))))
    (org-insert-property-drawer)
    (cl-loop for ent in entry
          do
          (when (not (string-equal (car ent) "abstract"))
            (org-set-property (upcase (car ent))
                              (ref-man--replace-non-ascii (ref-man--fix-curly (cdr ent))))))
    (org-set-property "CUSTOM_ID" key)
    (if (string-equal (assoc "type" entry) "misc")
        (org-set-property "BTYPE" "misc")
      (org-set-property "BTYPE" "article"))))

;; TODO: Rename this properly
(defun ref-man--org-bibtex-write-ref-from-ss-ref (entry &optional ignore-errors update-current)
  "Generate an org entry from data fetched from Semantic Scholar.
ENTRY is an alist of symbols cons.  Optional IGNORE-ERRORS is in
case error occurs while parsing the org properties as bibtex.
With optional UPDATE-CURRENT, update the current org entry
properties (reflecting the bibliography data) with
semanticscholar data also.  The default is to insert a new entry
after current."
  ;; NOTE: insert only when title exists
  (when (cdass 'title entry)
    (unless update-current
      (org-insert-heading-after-current))
    (org-edit-headline (cdass 'title entry))
    ;; NOTE: insert heading only when not updating current heading
    (when (assoc 'abstract entry)
      (ref-man-org-insert-abstract (cdass 'abstract entry)))
    (insert "\n")
    (let ((author-str (mapconcat (lambda (x)
                                   (cdass 'name x))
                                 (cdass 'authors entry) ", ")))
      (org-indent-line)
      (insert (format "- Authors: %s" author-str))
      (org-insert-item)
      (insert (concat (cdass 'venue entry) ", " (format "%s" (cdass 'year entry))))
      (org-insert-property-drawer)
      (cl-loop for ent in entry
            do
            (cond ((or (eq (car ent) 'author) (eq (car ent) 'authors))
                   (when (not (string-empty-p author-str))
                     (org-set-property "AUTHOR"
                                       (ref-man--replace-non-ascii
                                        (ref-man--fix-curly
                                         (ref-man--build-bib-author author-str))))))
                  ((and (eq (car ent) 'isInfluential)
                        (eq (cdr ent) 't))
                   (outline-back-to-heading)
                   (org-set-tags ":influential:")
                   ;; (when (string-match-p "true" (downcase (format "%s" (cdr ent))))
                   ;;   (org-set-tags ":influential:"))
                   )
                  ((and (eq (car ent) 'url) update-current)
                   (org-set-property "SS_URL"
                                     (ref-man--replace-non-ascii
                                      (ref-man--fix-curly (format "%s" (cdr ent))))))
                  ((and (not (member (car ent) '(abstract references citations corpusId
                                                          fieldsOfStudy is_open_access
                                                          topics is_publisher_licensed)))
                        (cdr ent))
                   (org-set-property (upcase (symbol-name (car ent)))
                                     (ref-man--replace-non-ascii
                                      (ref-man--fix-curly (format "%s" (cdr ent))))))))
      (let ((key (ref-man-parse-bib-property-key)))
        (unless (or key ignore-errors)
          (debug)
          (setq key (read-from-minibuffer (format "Could not parse key:\nauthor: %s\ntitle: %s\nyear: %s"
                                                  (org-entry-get (point) "AUTHOR")
                                                  (org-entry-get (point) "TITLE")
                                                  (org-entry-get (point) "YEAR")))))
        (when key
          (org-set-property "CUSTOM_ID" key)))
      (org-set-property "BTYPE" "article"))
    (when update-current
      (unless (org-at-heading-p)
        (outline-previous-heading))
      (beginning-of-line)
      (forward-whitespace 1)
      (just-one-space)
      (unless (eolp)
        (kill-line))
      (insert (cdr (assoc 'title entry))))))

(defun ref-man--org-bibtex-write-ref-from-assoc (entry &optional ignore-errors)
  "Write an org entry from an alist ENTRY with string cons'es.
Optional non-nil IGNORE-ERRORS is unused to conform with all the
org writing functions."
  (let* ((key (car entry))
         (entry (nth 1 entry)))
    (org-insert-heading-after-current)
    (insert (cdr (assoc "title" entry)))
    (insert "\n")
    (org-indent-line)
    (when (assoc "abstract" entry)
        (insert (cdr (assoc "abstract" entry)))
        (fill-paragraph)
        (insert "\n")
        (org-indent-line))
    (insert (format "- Authors: %s"
                    (ref-man--build-vernacular-author
                     (ref-man--replace-non-ascii (cdr (assoc "author" entry))))))
    (org-insert-item)
    (insert (concat (cdr (assoc "venue" entry)) ", " (cdr (assoc "year" entry))))
    (org-insert-property-drawer)
    (cl-loop for ent in entry
          do
          (when (not (string-equal (car ent) "abstract"))
            (org-set-property (upcase (car ent))
                              (ref-man--replace-non-ascii (ref-man--fix-curly (cdr ent))))))
    (org-set-property "CUSTOM_ID" key)
    (org-set-property "BTYPE" "article")))

(defun ref-man--org-bibtex-write-ref-from-plist (entry &optional ignore-errors)
  "Write an org entry from an plist ENTRY.
Optional non-nil IGNORE-ERRORS is unused to conform with all the
org writing functions."
  (org-insert-heading-after-current)
  (insert (cdr (assoc :title entry)))
  (insert "\n")
  (org-indent-line)
  (when (assoc :abstract entry)
    (insert (cdr (assoc :abstract entry)))
    (fill-paragraph)
    (insert "\n")
    (org-indent-line))
  (insert (format "- Authors: %s"
                  (ref-man--build-vernacular-author
                   (ref-man--replace-non-ascii (cdr (assoc :author entry))))))
  (org-insert-item)
  (insert (concat (if (cdr (assoc :venue entry)) (cdr (assoc :venue entry)) "NO VENUE")
                  ", " (cdr (assoc :year entry))))
  (org-insert-property-drawer)
  (cl-loop for ent in entry
        do
        (when (and (not (eq (car ent) :abstract))
                   (not (eq (car ent) :key)))
          (org-set-property (upcase (string-remove-prefix ":" (format "%s" (car ent))))
                            (ref-man--replace-non-ascii (ref-man--fix-curly (cdr ent))))))
  (let ((key (ref-man-parse-bib-property-key)))
    (unless key
      (setq key (read-from-minibuffer (format "Could not parse key:\nauthor: %s\ntitle: %s\nyear: %s"
                                              (org-entry-get (point) "AUTHOR")
                                              (org-entry-get (point) "TITLE")
                                              (org-entry-get (point) "YEAR")))))
    (org-set-property "CUSTOM_ID" key))
  (org-set-property "BTYPE" "article"))

;; FIXME: I don't know if this function is ever used
;; (defun ref-man--org-bibtex-write-ref-from-vector (entry)
;;   "Generate an org entry from an association list retrieved via
;; json."
;;   (let* ((key (car entry))
;;          (entry (nth 1 entry)))
;;     (org-insert-heading-after-current)
;;     (insert (cdr (assoc "title" entry)))
;;     (insert "\n")
;;     (org-indent-line)
;;     (when (assoc "abstract" entry)
;;         (insert (cdr (assoc "abstract" entry)))
;;         (fill-paragraph)
;;         (insert "\n")
;;         (org-indent-line))
;;     (insert (format "- Authors: %s"
;;                     (ref-man--build-vernacular-author (ref-man--replace-non-ascii (cdr (assoc "author" entry))))))
;;     (org-insert-item)
;;     (insert (concat (cdr (assoc "venue" entry)) ", " (cdr (assoc "year" entry))))
;;     (org-insert-property-drawer)
;;     (cl-loop for ent in entry
;;           do
;;           (when (not (string-equal (car ent) "abstract"))
;;             (org-set-property (upcase (car ent))
;;                               (ref-man--replace-non-ascii (ref-man--fix-curly (cdr ent))))))
;;     (org-set-property "CUSTOM_ID" key)
;;     (org-set-property "BTYPE" "article")))

(defun ref-man--org-bibtex-write-heading-from-bibtex (entry &optional ignore-errors)
  "Write an org entry from an alist ENTRY.
The alist is generated from `bibtex-parse-entry', probably from a
bibtex buffer.  Optional non-nil IGNORE-ERRORS is unused to
conform with all the org writing functions."
  (org-insert-heading)
  (insert (ref-man--fix-curly (cdr (assoc "title" entry))))
  (insert "\n")
  ;; from where do I get the abstract?
  ;; assuming abstract is in the bib entry
  (when (assoc "abstract" entry)
      (insert (ref-man--fix-curly (cdr (assoc "abstract" entry))))
      (org-indent-line)
      (fill-paragraph))
  (org-insert-property-drawer)
  (cl-loop for ent in entry
        do
        (pcase ent
          (`("abstract" . ,_))
          (`("=type=" . ,_) (org-set-property "BTYPE" (ref-man--fix-curly (cdr ent))))
          (`("=key=" . ,_) (org-set-property "CUSTOM_ID" (ref-man--fix-curly (cdr ent))))
          (`(,_ . ,_) (org-set-property (upcase (car ent)) (ref-man--fix-curly (cdr ent)))))))

(defun ref-man--bibtex-parse-buffer (buf)
  "Parse a bibtex buffer BUF and return results."
  (let (entries)
    (with-current-buffer buf
      (goto-char (point-min))
      (while (search-forward "@" nil t)
        (backward-char)
        (push (bibtex-parse-entry t) entries)))
    entries))

(defun ref-man-org-bibtex-read-bib-file-to-org-buffer (filename &optional buffername)
  "Parse a bibtex file FILENAME and convert it into an org buffer.
The org entries correspond to the bib entries in the bibtex
file.  If optional BUFFERNAME is given, the entries are appended
to that buffer else, a buffer is opeened with visiting-filename
FILENAME where the suffix .bib is replaced with .org."
  (interactive (list (ido-read-file-name "Bib File: ")))
  (when current-prefix-arg
    (setq buffername
          (if (eq major-mode 'org-mode)
              (buffer-name)
            (ido-completing-read "Org buffer: "
                                 (mapcar (lambda (x) (format "%s" x)) (buffer-list))))))
  (if (file-exists-p filename)
      (let* ((org-buf
              (if buffername (get-buffer buffername)
                (ref-man--create-org-buffer
                 (replace-regexp-in-string
                  "\\.[a-z0-9]*$" ".org" filename))))
             (entries (ref-man--bibtex-parse-buffer (find-file-noselect filename 'nowarn))))
        ;; (setq org-bibtex-entries nil)
        ;; (org-bibtex-read-buffer (find-file-noselect filename 'nowarn))
        (with-current-buffer org-buf
          (ref-man--insert-refs-from-seq
           entries nil 'bibtex)))
    (message (format "[ref-man] File %s does not exist" filename))))

(defun ref-man-org-bibtex-kill-headline-as-bibtex ()
  "Parse the headline at point, convert to a bib entry and append to `kill-ring'.
Only for interactive use.  Same as
`ref-man-org-bibtex-read-from-headline' except it returns nothing
and kills the entry as bibtex."
  (ref-man-org-bibtex-read-from-headline t))

(defun ref-man-org-bibtex-read-from-headline (&optional kill)
  "Parse the headline at point and convert to a bib entry.
The entry is appended to `ref-man-bibtex-save-ring'.  When
optional KILL is non-nill, the entry is also added to `kill-ring'
with `kill-new'."
  (interactive)
  (if (eq major-mode 'org-mode)
      (let* ((props (org-entry-properties))
             (bib-str (list
                       (cons "type"  (concat "@" (cdr (assoc "BTYPE" props))))
                       (cons "key"  (cdr (assoc "CUSTOM_ID" props)))
                       (cons "title"  (cdr (assoc "TITLE" props)))
                       (cons "author"  (cdr (assoc "AUTHOR" props)))
                       (cons "venue"  (cdr (assoc "VENUE" props)))
                       (cons "booktitle"  (cdr (assoc "BOOKTITLE" props)))
                       (cons "volume"  (cdr (assoc "VOLUME" props)))
                       (cons "number"  (cdr (assoc "NUMBER" props)))
                       (cons "month"  (cdr (assoc "MONTH" props)))
                       (cons "year"  (cdr (assoc "YEAR" props)))
                       (cons "pages"  (cdr (assoc "PAGES" props)))
                       (cons "doi"  (cdr (assoc "DOI" props)))
                       (cons "url"  (cdr (assoc "URL" props)))
                       (cons "publisher"  (cdr (assoc "PUBLISHER" props)))
                       (cons "organization"  (cdr (assoc "ORGANIZATION" props)))))
             (header (concat (cdr (assoc "type" bib-str)) "{" (cdr (assoc "key" bib-str)) ",\n"))
             (bib-str (delq (assoc "type" bib-str) bib-str))
             (bib-str (delq (assoc "key" bib-str) bib-str))
             (bib-str (concat header
                              (mapconcat (lambda (x)
                                           (if (cdr x) (concat "  " (car x) "={" (cdr x) "},\n")))
                                         bib-str "") "}\n")))
        (when kill
          (kill-new bib-str))
        (push bib-str ref-man-bibtex-save-ring)
        bib-str)
    (message "[ref-man] Not org mode") nil))

;; TODO: prefix arg should insert to temp-file in interactive mode
(defun ref-man-org-bibtex-kill-or-insert-headline-as-bib-to-file (&optional file)
  "Export current headline as bibtex.

Where to export depends on various facts.  When optional FILE is
non-nil, get the current buffer visiting that file use that, else
find the file and open it.  If FILE is not given then insert to
`ref-man-temp-bib-file-path'.

In all cases the bibtex entry is inserted at the top of the
buffer."
  (interactive)
  (let* ((result (ref-man-org-bibtex-read-from-headline)))
    (if file
        (let* ((bib-file-path (if file file ref-man-temp-bib-file-path))
               (bib-file-name (file-name-nondirectory bib-file-path))
               (buf (if (get-buffer bib-file-name) (get-buffer bib-file-name)
                      (find-file-noselect bib-file-path))))
          (with-current-buffer buf
            (goto-char (point-min)) (insert result)
            (message (concat "Inserted entry to " bib-file-name))))
      (kill-new result)
      (message "Inserted entry to kill ring"))))

(defun ref-man-org-bibtex-yank-bib-to-property ()
  "Yank the `current-kill', parse as bibtex entry and update properties.
If some fields are present in both bibtex and property, they are
overwritten.  Fields not present in bib entry are not deleted."
  (interactive)
  (let ((bib-assoc (with-temp-buffer
                     (yank)
                     (goto-char (point-min))
                     (bibtex-parse-entry))))
    (ref-man-org-bibtex-convert-bib-to-property
     bib-assoc (current-buffer) (point) t)))

(defun ref-man-org-bibtex-dump-bib-to-property ()
  "Export the last bibtex entry read and update property drawer.
Like `ref-man-org-bibtex-yank-bib-to-property' but uses a
separate variable `ref-man-bibtex-save-ring' instead of `kill-ring'."
  (interactive)
  (let ((bib-assoc (with-temp-buffer
                     (insert (car ref-man-bibtex-save-ring))
                     (goto-char (point-min))
                     (bibtex-parse-entry))))
    (ref-man-org-bibtex-convert-bib-to-property
     bib-assoc (current-buffer) (point) t)))

(defun ref-man-org-bibtex-convert-bib-to-property (bib-alist &optional buf buf-point no-edit-headline)
  "Convert an alist BIB-ALIST parsed by bibtex to an org property drawer.
With optional BUF, the headling at (point) is updated.  When
BUF-POINT is non-nil, goto that point and update that entry.
NO-EDIT-HEADLINE specifies to only update the properties and not
the headline itself.  Default is to edit the headline also."
  (let ((buf (if buf buf (current-buffer)))
        (entry bib-alist)
        (buf-point (cond (buf-point buf-point)
                         ((not ref-man--org-gscholar-launch-point)
                          (with-current-buffer buf (point)))
                         (t ref-man--org-gscholar-launch-point))))
    (with-current-buffer buf
      (goto-char buf-point)
      (when (and (not no-edit-headline) (cdr (assoc "title" bib-alist)))
        (org-edit-headline (ref-man--trim-and-unquote
                            (ref-man--fix-curly (cdr (assoc "title" bib-alist))))))
      (cl-loop for ent in entry
            do
            (pcase ent
              (`("abstract" . ,_))
              (`("=type=" . ,_) (org-set-property "BTYPE" (ref-man--fix-curly (cdr ent))))
              ;; (`("=key=" . ,_) (org-set-property "CUSTOM_ID" (ref-man--fix-curly (cdr ent))))
              (`("=key=" . ,_) nil)     ; Ignore =key=
              (`("timestamp" . ,_) nil)     ; Ignore timestamp
              (`("author" . ,_) (org-set-property "AUTHOR" (ref-man--replace-non-ascii
                                                            (ref-man--trim-and-unquote
                                                             (ref-man--fix-curly (cdr ent))))))
              (`(,_ . ,_) (org-set-property (upcase (car ent)) (ref-man--replace-non-ascii
                                                                (ref-man--trim-and-unquote
                                                                 (ref-man--fix-curly (cdr ent))))))))
       ;; Put key generated by own rules
      (org-set-property "CUSTOM_ID" (ref-man-parse-bib-property-key)))))

;; TODO: "No property drawer" should not come when property drawer is present
;; CHECK: Why only called from `ref-man--parse-bibtex'?
(defun ref-man--sanitize-org-entry (&optional org-buf org-point)
  "Sanitize an org entry.
If optional ORG-BUF is given then sanitize the entry at current
point in that buffer.  Otherwise check if
`ref-man--org-gscholar-launch-buffer' is non-nil and use that as
ORG-BUF.

If both are non existent then check if current buffer is an org
buffer and sanitize the entry at point."
  (let (retval)
    (condition-case ex
        (setq retval
              (let* ((org-buf (cond (org-buf org-buf)
                                   (ref-man--org-gscholar-launch-buffer ref-man--org-gscholar-launch-buffer)
                                   (ref-man--org-gscholar-launch-buffer ref-man--org-gscholar-launch-buffer)))
                     (org-point (or org-point (and ref-man--org-gscholar-launch-buffer
                                                   ref-man--org-gscholar-launch-point)
                                    (with-current-buffer org-buf (point)))))
                (cond
                 ((not org-buf) "No suitable org buffer found")
                 ((not (equal (with-current-buffer org-buf major-mode)
                              'org-mode)) "Not org mode")
                 ((not (with-current-buffer org-buf
                         (org-entry-get org-point "CUSTOM_ID")))
                  (with-current-buffer org-buf
                    (org-set-property "CUSTOM_ID" "na_"))
                  "No property drawer or missing properties. Fixed")
                 (t (with-current-buffer org-buf
                      (org-entry-get org-point "CUSTOM_ID"))))))
      ('error (message (format "[ref-man] Caught exception: [%s]" ex))))
    (message (concat "[ref-man] " retval))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END Org generation and insertion stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START python process stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Always use venv and setup
;;
;; TODO: Check python3 version > 3.6.5
(defun ref-man-python-process-setup ()
  (when ref-man-python-process-use-venv
    (let ((env (path-join ref-man-home-dir "env")))
      (unless (and (f-exists? env) (f-exists? (path-join env "bin" "python3")))
        (shell-command (format "python3 -m virtualenv -p python3 %s" env)
                       "*ref-man-cmd*" "*ref-man-cmd*")
        (async-shell-command (concat "source " (path-join env "bin" "activate") " && "
                                     (format "cd %s && pip install ." ref-man-home-dir))
                             "*ref-man-cmd*" "*ref-man-cmd*")))))

;; TODO: Requests to python server should be dynamic according to whether I want
;;       to use proxy or not at that point
(defun ref-man--python-process-helper (data-dir port)
  "Start the python server.
DATA-DIR is the server data directory.  PORT is the port to which
the server binds.

When called from `ref-man-start-python-server', DATA-DIR is set
to `ref-man-python-data-dir' and the port
`ref-man-python-server-port'."
  ;; NOTE: Hack so that process isn't returned
  (prog1
      (message (format "[ref-man] Starting python process on port: %s"
                       ref-man-python-server-port))
    (let* ((env (and ref-man-python-process-use-venv
                     (path-join ref-man-home-dir "env")))
           (args (-filter #'identity (list (format "--data-dir=%s" data-dir)
                                          (format "--port=%s" port)
                                          (and ref-man-proxy-port "--proxy-everything")
                                          (and ref-man-proxy-port
                                               (format "--proxy-everything-port=%s"
                                                       ref-man-proxy-port))
                                          (and ref-man-pdf-proxy-port
                                               (format "--proxy-port=%s" ref-man-pdf-proxy-port))
                                          "--verbosity=debug")))
           (process-environment (if env
                                    (with-temp-buffer
                                      (call-process "bash" nil t nil "-c"
                                                    (concat "source " (path-join env "bin" "activate") "; env"))
                                      (goto-char (point-min))
                                      (let ((temp nil))
                                        (while (not (eobp))
                                          (setq temp
                                                (cons (buffer-substring (point) (line-end-position))
                                                      temp))
                                          (forward-line 1))
                                        temp))
                                  process-environment))
           (python (ref-man--trim-whitespace (shell-command-to-string "which python"))))
      ;; (message "Python process args are %s" args)
      (apply #'start-process "ref-man-python-server" "*ref-man-python-server*"
             python (path-join ref-man-home-dir "main.py") args))))

(defun ref-man-stop-python-server ()
  "Stop the python server by sending a shutdown command.
This is sent via http and lets the server exit gracefully."
  (interactive)
  (let ((buf (url-retrieve-synchronously
              (format "http://localhost:%s/shutdown" ref-man-python-server-port))))
    (with-current-buffer buf
      (goto-char (point-min))
      (re-search-forward "\r?\n\r?\n")
      (message (buffer-substring-no-properties (point) (point-max))))))

(defun ref-man--kill-internal-python-process ()
  "Kill the internal python process process by sending SIGKILL."
  (signal-process (get-buffer "*ref-man-python-server*") 15))

(defun ref-man--kill-external-python-process ()
  "Kill the external python process process by sending SIGKILL."
  (signal-process ref-man-external-python-process-pid 15))

(defun ref-man-python-server-reachable-p ()
  "Check if python server is reachable."
  (condition-case nil
      (let ((buf (url-retrieve-synchronously
                  (format "http://localhost:%s/version" ref-man-python-server-port) t)))
        (when buf
          (string-match-p "ref-man python server"
                          (with-current-buffer buf (buffer-string)))))
    (error nil)))

(defun ref-man-python-process-running ()
  "Check if python server is running.
Returns 'external or 'internal according to where the process is
running if it's running else nil."
  (cond ((get-buffer-process "*ref-man-python-server*")
         (setq ref-man-external-python-process-pid nil)
         'internal)
        ((ref-man-external-python-process-p)
         'external)
        (t nil)))

(defun ref-man-python-server-running ()
  "Check if python server is already running."
  (interactive)
  (let ((python-process (ref-man-python-process-running)))
    (when python-process
      (if (ref-man-python-server-reachable-p)
          python-process
        (if (eq python-process 'internal)
            'internal-error 'external-error)))))

(defun ref-man-external-python-process-p ()
  "Check for `server.py' python processes outside emacs.
In case a process is found, `ref-man-python-server-port' is set
to the port of that process and
`ref-man-external-python-process-pid' is set to its pid."
  (let ((python-strings
         (split-string (shell-command-to-string "ps -ef | grep python | grep server") "\n")))
    (cl-loop for x in python-strings
             do
             (when (and (string-match-p "port" x) (string-match-p "data-dir" x))
               (setq ref-man-python-server-port
                     (string-to-number
                      (cadr (split-string
                             (car (split-string
                                   (substring x (string-match "port" x)))) "="))))
               (setq ref-man-external-python-process-pid (string-to-number (nth 1 (split-string x))))
               (cl-return t)))))

(defun ref-man-start-python-server ()
  "Start the python server, unless already running.

The server can be running outside emacs also in which case
`ref-man-python-server-port' is set to port.

See accompanying `server.py' for the server details.  The API and
methods are still evolving but as of now it supports DBLP and
ArXiv.  The process if started opens a local port and can fetch
data in multiple threads from supported APIs before preprocessing
and consolidating.  It also maintains a local datastore."
  (interactive)
  ;; FIXME: for 'internal-error and 'external-error
  (if (ref-man-python-server-running)
      (message (format "Found existing process running on port: %s"
                       ref-man-python-server-port))
    (message "No existing python process found")
    (let ((port (find-open-port ref-man-python-server-port-start))
          (data-dir ref-man-python-data-dir))
      (setq ref-man-python-server-port port)
      (ref-man--python-process-helper data-dir port))))

(defun ref-man-restart-python-server ()
  "Restart the python server."
  (interactive)
  (cond ((or (eq 'internal (ref-man-python-server-running))
             (eq 'external (ref-man-python-server-running)))
         (ref-man-stop-python-server))
        ((eq 'internal-error (ref-man-python-server-running))
         (ref-man--kill-internal-python-process))
        ((eq 'external-error (ref-man-python-server-running))
         (ref-man--kill-external-python-process)))
  ;; FIXME: This runs before server shuts down
  (ref-man-start-python-server))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END python process stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;; START Biblio stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(defun biblio-crossref-backend (arg)
  "Not implemented ARG.")

(defun ref-man-org-search-heading-on-crossref-with-biblio ()
  "Search for the heading at point on crossref with `biblio'."
  (interactive)
  (setq ref-man--org-gscholar-launch-point (point))
  (setq ref-man--org-gscholar-launch-buffer (current-buffer))
  (save-excursion
    (let* ((query (org-get-heading t t))
           (target-buffer (window-buffer (ref-man--get-or-create-window-on-side)))
           (backend #'biblio-crossref-backend)
           (results-buffer (biblio--make-results-buffer target-buffer query backend)))
      (biblio-url-retrieve
       (funcall backend 'url query)
       (ref-man--biblio-callback results-buffer backend))
      results-buffer)))
(make-obsolete 'ref-man-org-search-heading-on-crossref-with-biblio nil "ref-man 0.3.0")

(defun ref-man--biblio-callback (results-buffer backend)
  "Generate a search results callback for RESULTS-BUFFER.
Results are parsed with (BACKEND 'parse-buffer)."
  ;; TODO: Did let not work here?
  ;;       Gotta check.
  (setq ref-man--biblio-callback-buf results-buffer)
  (biblio-generic-url-callback
   (lambda () ;; no allowed errors, so no arguments
     "Parse results of bibliographic search."
     (let ((results (biblio--tag-backend backend
                                         (funcall backend 'parse-buffer)))
           (win (ref-man--get-or-create-window-on-side)))
       (with-current-buffer ref-man--biblio-callback-buf
         (ref-man--biblio-insert-results results (biblio--search-results-header))
         (local-set-key (kbd "n") 'biblio--selection-next)
         (local-set-key (kbd "p") 'biblio--selection-previous)
         (local-set-key (kbd "o") 'ref-man--parse-selected-biblio-entry-to-org))
       (set-window-buffer win ref-man--biblio-callback-buf)
       (message "[ref-man] Tip: learn to browse results with `h'")))))

(defun ref-man--parse-selected-biblio-entry-to-org ()
  "Parse the selected biblio entry to an org buffer.
Org buffer defaults to `ref-man--org-gscholar-launch-buffer'."
  (interactive)
  (biblio--selection-forward-bibtex #'ref-man--biblio-insert-to-org))

(defun ref-man--biblio-insert-to-org (bibtex)
  "Insert bibtex entry BIBTEX parsed by biblio to org buffer.
Org buffer defaults to `ref-man--org-gscholar-launch-buffer'."
  (let* ((current-key (with-current-buffer ref-man--org-gscholar-launch-buffer
                        (org-entry-get ref-man--org-gscholar-launch-point "CUSTOM_ID")))
         (bibtex (replace-in-string (replace-in-string
                                     (progn (set-text-properties 0 (length bibtex) nil bibtex) bibtex)
                                     "\n" "") "[[:blank:]]+" " "))
         (bib-assoc (with-temp-buffer (insert bibtex)
                                      (goto-char (point-min))
                                      (bibtex-parse-entry)))
         (new-key (ref-man--build-bib-key-from-parsed-bibtex bib-assoc))
         (bib-assoc (-remove 'ref-man--bibtex-key-p bib-assoc)))
    (setf (alist-get "=key=" bib-assoc) new-key)
    (cond ((not bib-assoc) (message "[ref-man] Received nil entry"))
          ((string-match-p "na_" current-key)
           (ref-man-org-bibtex-convert-bib-to-property bib-assoc ref-man--org-gscholar-launch-buffer))
          ((y-or-n-p "Authoritative entry.  Really replace? ")
           (ref-man-org-bibtex-convert-bib-to-property bib-assoc ref-man--org-gscholar-launch-buffer)))
    (pop-to-buffer ref-man--org-gscholar-launch-buffer)))

(defun ref-man--biblio-insert-results (items &optional header)
  "Populate current buffer with ITEMS and HEADER, then display it."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (biblio--insert-header header)
    (seq-do #'biblio-insert-result items))
  (biblio--selection-first)
  (hl-line-highlight))
;;;;;;;;;;;;;;;;;;;;;;
;; END Biblio stuff ;;
;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START org utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ref-man-org-clear-property-drawer (&optional org-buf pt)
  "Clear the property drawer at point in buffer ORG-BUF.
Optional PT is the point to go to before clearing the drawer,
defaults to current point `point'."
  (save-excursion
    (unless org-buf
      (setq org-buf (current-buffer)))
    (with-current-buffer org-buf
      (let ((bounds (org-get-property-block pt)))
        (when bounds
          (delete-region (car bounds) (cdr bounds)))))))

;; TODO: Can add more modes later.
(defun ref-man-get-title-according-to-mode (user-input)
  "Get the title at point according to `major-mode'.
In `org-mode' the heading is returned.  In `bibtex-mode' the title
field is returned.  If no mode matches or USER-INPUT is non-nil,
then read it from minibuffer."
  (interactive)
  (if user-input
      (read-string
       "[ref-man-chrome] Enter the string to search: ")
    (cond ((eq major-mode 'org-mode)
           (setq ref-man--org-gscholar-launch-point (point)) ; CHECK: must be at heading?
           (setq ref-man--org-gscholar-launch-buffer (current-buffer))
           (substring-no-properties (org-get-heading t t)))
          ((eq major-mode 'bibtex-mode)
           (bibtex-autokey-get-field "title"))
          (t (read-string
              "[ref-man] Enter the string to search: ")))))

(defun ref-man--org-property-is-url-p (prop)
  "Check if PROP is a url."
  (and (listp prop) (string-equal (symbol-name (car prop)) ":uri")))

;; From https://emacs.stackexchange.com/a/16914
;; TODO: Make this async for large buffers
;;       Or perhaps cache the results
;; Not really hidden

(defun ref-man-org-insert-link-as-headline (org-buf link title metadata current)
  "Insert LINK as org headline to ORG-BUF.
LINK-TEXT is as given in the html buffer.  METADATA can be either
a string or an alist of author, title and venue strings.  If
CURRENT is non-nil, update current heading, otherwise insert a
new heading.

The function returns a plist with keys :buffer :heading :point
which can be used to further add data to the org heading."
  (save-excursion
    (with-current-buffer org-buf
      (cond ((f-equal? (buffer-file-name org-buf)
                       ref-man-org-links-file-path)
             (org-mode)
             (org-datetree-find-date-create (org-date-to-gregorian (org-read-date t nil "now")))
             (goto-char (point-at-eol))
             (org-insert-subheading nil))
            ((eq ref-man--org-gscholar-launch-buffer org-buf)
             (goto-char ref-man--org-gscholar-launch-point)
             ;; (goto-char (line-end-position))
             (unless current
               (end-of-line)
               (org-insert-heading-respect-content)
               (org-do-demote)))
            (t nil))
      (org-edit-headline title)
      (end-of-line)
      (let ((pblock (org-get-property-block)))
        (when pblock
          (goto-char (cdr pblock))
          (end-of-line)))
      (newline-and-indent)
      (insert (concat "- [[" link "][link]]"))
      (if (stringp metadata)
          (progn (newline-and-indent)
                 (org-insert-item)
                 (insert metadata))
        (seq-do (lambda (x)
                  (newline-and-indent)
                  (org-insert-item)
                  (insert (capitalize (symbol-name (car x))) ": " (replace-regexp-in-string "\n" " " (cdr x)))
                  (fill-paragraph))
                metadata))
      (message (concat "[ref-man] " (format "Imported entry \"%s\" into buffer <%s>" title (buffer-name org-buf))))
      ;; restore ref-man--org-gscholar-launch-point but return point
      (prog1 (list :buffer (current-buffer)
                   :heading (substring-no-properties (org-get-heading))
                   :point (point))
        (when (and ref-man--org-gscholar-launch-buffer ref-man--org-gscholar-launch-point)
          (goto-char ref-man--org-gscholar-launch-point))))))

(defun ref-man--insert-org-pdf-url-property (url)
  "Insert FILE as property to an org drawer.
Confirm in case the property PDF_FILE exists."
  (org-set-property "PDF_URL" url)
  ;; NOTE: Maybe asking for this is too much
  ;; (let ((props (org-entry-properties)))
  ;;   (if (and props (cdr (assoc "PDF_URL" props)))
  ;;       (if (y-or-n-p "PDF_URL already exists.  Replace? ")
  ;;           (org-set-property "PDF_URL" url)
  ;;         (message "[ref-man] Not overwriting PDF_URL."))
  ;;     (org-set-property "PDF_URL" url)))
  )

(defun ref-man--insert-org-pdf-file-property (file)
  "Insert FILE as property to an org drawer.
Confirm in case the property PDF_FILE exists."
  (let ((props (org-entry-properties))
        (file-entry (concat "[[" file "]]")))
    (if (and props (cdr (assoc "PDF_FILE" props)))
        (if (y-or-n-p (format "PDF_FILE %s already exists in props.  Replace? "
                              (f-filename file)))
            (org-set-property "PDF_FILE" file-entry)
          (message "[ref-man] Not overwriting PDF_FILE."))
      (org-set-property "PDF_FILE" file-entry))))

(defun ref-man--eww-pdf-download-callback-store-new (status url args)
  "Callback for `url-retrieve' pdf download.
STATUS is response status (I think).  URL is the url.  PT if
given stores the point in `ref-man--subtree-list'.  This callback
is meant to operate in batch mode."
  (unless (plist-get status :error)
    (let ((file (ref-man-files-filename-from-url url))
          (buf (and args (plist-get args :buffer)))
          (pt (and args (plist-get args :point)))
          (buf-type (ref-man--check-response-buffer (current-buffer)))
          (heading (and args (plist-get args :heading))))
      (cond ((eq buf-type 'html)
             (message (format "[ref-man] Got html buffer for url %s. Not saving file" url)))
            ((eq buf-type 'pdf)
             (write-region (point) (point-max) file)
             (message "[ref-man] Saved %s" file)))
      (if buf
          (with-current-buffer buf
            (cond ((string-empty-p (string-trim (replace-regexp-in-string "\*" "" heading)))
                   (goto-char pt)
                   (when ref-man-update-pdf-url-when-download
                     (ref-man--insert-org-pdf-url-property
                      (ref-man-url-maybe-unproxy url)))
                   (when (eq buf-type 'pdf)
                     (ref-man--insert-org-pdf-file-property file)))
                  (t
                   (save-excursion
                     (org-link-search heading)
                     (when ref-man-update-pdf-url-when-download
                       (ref-man--insert-org-pdf-url-property
                        (ref-man-url-maybe-unproxy url)))
                     (when (eq buf-type 'pdf)
                       (ref-man--insert-org-pdf-file-property file))))))
        (with-current-buffer ref-man--org-gscholar-launch-buffer
          (save-excursion
            (goto-char ref-man--org-gscholar-launch-point)
            (when ref-man-update-pdf-url-when-download
              (ref-man--insert-org-pdf-url-property
               (ref-man-url-maybe-unproxy url)))
            (when (eq buf-type 'pdf)
              (ref-man--insert-org-pdf-file-property file))))))))

;; CHECK: Not sure if the two functions below should be here
(defun ref-man--eww-pdf-download-callback-store (status url pt)
  "Callback for `url-retrieve' pdf download.
STATUS is response status (I think).  URL is the url.  PT if
given stores the point in `ref-man--subtree-list'.  This callback
is meant to operate in batch mode."
  (unless (plist-get status :error)
    (let ((file (ref-man-files-filename-from-url url)))
        (goto-char (point-min))
        (re-search-forward "\r?\n\r?\n")
        (write-region (point) (point-max) file)
        (message "[ref-man] Saved %s" file)
        (when pt
          ;; NOTE: Does nothing
          )
        (let* ((elem (-first (lambda (x) (string= (plist-get x :url) url)) ref-man--subtree-list))
               (repl (plist-put elem :file file)))
          (setq ref-man--subtree-list (-replace-first elem repl ref-man--subtree-list)))
        ;; (with-current-buffer ref-man--org-gscholar-launch-buffer
        ;;   (save-excursion
        ;;     (goto-char ref-man--org-gscholar-launch-point)
        ;;     (ref-man--insert-org-pdf-file-property file)))
        ;; (setq ref-man--subtree-list
        ;;       (plist-put (plist-put ref-man--subtree-list :url url) :file file))
        )))

(defun ref-man--check-response-buffer (buf)
  "Check if buffer BUF is json or pdf.
Examine buffer for certain characters % and { as a heuristic."
  (with-current-buffer buf
    (goto-char (point-min))
    (forward-paragraph)
    (skip-chars-forward " \t\r\n")
    (let ((char (char-after (point))))
      (cond ((eq 37 char) 'pdf)
            ((eq 123 char) 'json)
            ((eq 60 char) 'html)
            (t char)))))

(defun ref-man--handle-json-response (json-data &optional storep)
  "Handle JSON response from `url-retrieve'.
JSON-DATA is parsed and sent by `ref-man--eww-pdf-download-callback'.

If it's a 'redirect then ask user if we should follow the
redirect.  Optional argument STOREP is for batch mode."
  (when (equal '(redirect content) (-map #'car json-data))
    (let ((pdf-url (ref-man-url-get-pdf-url-according-to-source (cdass 'redirect json-data))))
      (cond ((and pdf-url (y-or-n-p (format "Got redirect to %s.  Fetch? " pdf-url)) (not storep))
             (ref-man--fetch-from-pdf-url pdf-url))
            ((and pdf-url storep)
             (ref-man--fetch-from-pdf-url pdf-url storep))
            ((not pdf-url)
             (message "[ref-man] handle-json-response, not a PDF URL in redirect.")
             (unless storep (debug)))
            (t (message "[ref-man] handle-json-response, not sure what to do"))))))

(defun ref-man--eww-pdf-download-callback (status url &optional view overwrite)
  "Callback for `url-retrieve' pdf download.
STATUS is response status (I think).  URL is the url.

With optional argument VIEW non-nil, view the pdf file also.
OVERWRITE specifies to overwrite the target pdf file without
confirmation if it exists."
  (if (plist-get status :error)
      (message (format "[ref-man] Error occured while download %s" url))
    (let ((file (ref-man-files-filename-from-url url))
          (buf-type (ref-man--check-response-buffer (current-buffer))))
      (goto-char (point-min))
      (re-search-forward "\r?\n\r?\n")
      (goto-char (- (re-search-forward "[^\r?\n\r?\n]") 1))
      (cond ((string-match-p "bad request" (buffer-substring (point-min) (point)))
             (debug))
            ((eq buf-type 'pdf)
             (when ref-man--org-gscholar-launch-buffer
               (with-current-buffer ref-man--org-gscholar-launch-buffer
                 (save-excursion
                   (goto-char ref-man--org-gscholar-launch-point)
                   (ref-man--insert-org-pdf-file-property file))))
             (if (or overwrite (file-exists-p file))
                 (when (y-or-n-p "File exists.  Replace? ")
                   (write-region (point) (point-max) file)
                   (message "[ref-man] Saved %s" file))
               (write-region (point) (point-max) file)
               (message "[ref-man] Saved %s" file)
               ;; (ref-man-maybe-create-or-insert-org-heading-and-property file)
               )
             (when view (find-file-other-window file)))
            ((eq buf-type 'json)
             (ref-man--handle-json-response (json-read)))
            (t (debug))))))

(defun ref-man--pdf-copy-file (url &optional view overwrite)
  "Experimental file to copy pdf directly from URL.
Optional VIEW to view pdf after download and OVERWRITE to
overwrite if it exists without confirmation."
  (let ((file (ref-man-files-filename-from-url url)))
    (when ref-man--org-gscholar-launch-buffer
      (with-current-buffer ref-man--org-gscholar-launch-buffer
        (save-excursion
          (goto-char ref-man--org-gscholar-launch-point)
          (ref-man--insert-org-pdf-file-property file))))
    (if (or overwrite (file-exists-p file))
        (when (y-or-n-p "File exists.  Replace? ")
          (url-copy-file url file t)
          (message "[ref-man] Saved %s" file))
      (url-copy-file url file)
      (message "[ref-man] Saved %s" file))
    (when view (find-file-other-window file))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START org link functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ref-man-org-heading-level (&optional point)
  "Get level of current heading."
  (if (eq major-mode 'org-mode)
      (save-restriction
        (ref-man-org-narrow-to-heading-and-body)
        (save-excursion
          (when point
            (goto-char point))
          (let ((heading (car (with-current-buffer (current-buffer)
                                (save-restriction
                                  (org-narrow-to-subtree)
                                  (org-element-map (org-element-parse-buffer 'headline) 'headline
                                    (lambda (x) x)))))))
            (org-element-property :level heading))))
    (message "[ref-man] Not in org-mode")))

(defun ref-man-org-get-links-with-conditions (buf conditions &optional narrow raw)
  "Get all links from buffer BUF which satisfy CONDITONS.
CONDITIONS is either a unary boolean function or list of unary
boolean functions.  If a link is true for all CONDITIONS then add
link to result.  If CONDITIONS is nil then all links are
returned.

With optional argument NARROW is nil search parse the entire
buffer.  When non-nil narrow according to is value.  With value
'subtree narrow to subtree.  With value 'heading then narrow
according to `ref-man-org-narrow-to-heading-and-body'

Links are returned as an `org-element-type'.  If optional
argument RAW is non-nil, return the string value of the link
instead."
  (with-current-buffer buf
    (if (eq major-mode 'org-mode)
        (save-restriction
          (pcase narrow
            ('subtree (org-narrow-to-subtree))
            ('heading (ref-man-org-narrow-to-heading-and-body))
            ('nil)                      ; do nothing for nil
            (_ (org-narrow-to-subtree)))
          (org-element-map (org-element-parse-buffer) 'link
            (lambda (link)
              (cond ((functionp conditions)
                     (when (funcall conditions link)
                       (if raw
                           (org-element-property :raw-link link)
                         link)))
                    ((listp conditions)
                     (when (-all? #'identity (mapcar (lambda (pred) (funcall pred link)) conditions))
                       (if raw
                           (org-element-property :raw-link link)
                         link)))
                    (t link)))))
      (message "[ref-man] Not in org-mode") nil)))

(defun ref-man-org-get-links-of-type (buf link-re &optional narrow link-type)
  "Retrieves all links whose `type' matches LINK-RE from buffer BUF.
LINK-RE is the regexp to match against the link type.  For
example `http' matches `http' and `https' etc., while `^http$'
matches only `http'.

Optional NARROW works the same as in `ref-man-org-get-links-with-conditions'.

Optional LINK-TYPE specifies what to return.  When nil only the
raw-link is returned. When it equals 'path then return only the
path value. Helpful for `file' links sometimes.

is non-nil only the link path is
returned. "
  (let ((preds (lambda (link) (string-match-p link-re (org-element-property :type link)))))
    (pcase link-type
      ('path (mapcar (lambda (link) (org-element-property :path link))
                     (ref-man-org-get-links-with-conditions buf preds narrow)))
      ((or 'text 'raw) (ref-man-org-get-links-with-conditions buf preds narrow t))
      (_ (ref-man-org-get-links-with-conditions buf preds narrow)))))

(defun ref-man-org-get-number-of-http-links-from-org-buffer (&optional narrow)
  "Get the total number of http links from org heading text.
With optional NARROW, narrow to subtree."
  (save-restriction
    (when narrow (org-narrow-to-subtree))
    (length (org-element-map (org-element-parse-buffer) 'link
              (lambda (link)
                (when (string-match-p "[http|https]" (org-element-property :type link))
                  (org-element-property :raw-link link)))))))

(defun ref-man-org-get-first-link-from-org-heading ()
  "Get first http link an from org heading text at point."
  (save-restriction
    (ref-man-org-narrow-to-heading-and-body)
    (car (org-element-map (org-element-parse-buffer) 'link
           (lambda (link)
             (when (string-match-p "^[http|https]" (org-element-property :type link))
               link))))))

(defun ref-man--move-only-link-to-org-property-drawer ()
  "Move an only http link from org heading text to property drawer.
If more than one link exist the behaviour is undefined LOL.  Link
is saved to URL property."
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (when (string-match-p "^[http|https]" (org-element-property :type link))
            (let ((link-str (org-element-property :raw-link link)))
              (delete-region (org-element-property :begin link)
                             (org-element-property :end link))
              (when (and (org-at-item-p)
                         (not (string-match-p
                               "[^- ]" (buffer-substring-no-properties (point-at-bol)
                                                                       (point-at-eol)))))
                (delete-region (point-at-bol) (+ 1 (point-at-eol))))
              (org-set-property "URL" link-str))))))))

(defun ref-man--move-first-link-to-org-property-drawer ()
  "Move first http link from org heading text under point to property drawer.
Link is saved to URL property."
  (cl-block func
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (org-element-map (org-element-parse-buffer) 'link
          (lambda (link)
            (when (string-match-p "^[http|https]" (org-element-property :type link))
              (let ((url (org-element-property :raw-link link)))
                (delete-region (org-element-property :begin link)
                               (org-element-property :end link))
                (when (and (org-at-item-p)
                           (not (string-match-p
                                 "[^- ]" (buffer-substring-no-properties (point-at-bol)
                                                                         (point-at-eol)))))
                  (delete-region (point-at-bol) (+ 1 (point-at-eol))))
                (org-set-property "URL" url)
                (cl-return-from func t)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END org link functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ref-man--bib-buf-for-arxiv-api (url)
  "Fetch bibliography from an arxiv URL and return the buffer.
This function uses arxiv api with python server as an intermediary."
  (let* ((arxiv-id (ref-man-url-to-arxiv-id url))
         (bib-url (format "http://localhost:%s/arxiv?id=%s" ref-man-python-server-port arxiv-id)))
    (message (format "[ref-man] Fetching for arxiv-id %s" arxiv-id))
    (with-current-buffer
        (url-retrieve-synchronously bib-url)
      (goto-char (point-min))
      (forward-paragraph)
      (let ((bib-str (json-read)))
        (goto-char (point-min))
        (erase-buffer)
        (insert bib-str))
      (current-buffer))))

(defun ref-man--try-fetch-bib-buf-for-url (url)
  "Fetch bib buffer if possible for the given URL."
  (cond ((string-match-p "arxiv.org" url)
         (let* ((link (ref-man-url-get-bibtex-link-from-arxiv url)))
           (if link
               (url-retrieve-synchronously link)
             (ref-man--bib-buf-for-arxiv-api url))))
        (t
         (message (format "[ref-man] No way to get bib buffer for %s" url))
         nil)))

(defun ref-man--ss-id ()
  "Get one of possible IDs to fetch from Semantic Scholar.
Return a cons of `id-type' and `id'.  Possible values for
`id-type' are (\"ss\" \"doi\" \"arxiv\" \"acl\" \"mag\"
\"pubmed\" \"corpus\").

\"mag\" \"pubmed\" \"corpus\" aren't implemented yet."
  (ref-man--check-fix-url-property)
  (cond ((org-entry-get (point) "PAPERID")
         (cons 'ss (org-entry-get (point) "PAPERID")))
        ((org-entry-get (point) "DOI")
         (cons 'doi (org-entry-get (point) "DOI")))
        ((org-entry-get (point) "ARXIVID")
         (cons 'arxiv (org-entry-get (point) "ARXIVID")))
        ((org-entry-get (point) "EPRINT")
         (cons 'arxiv (org-entry-get (point) "EPRINT")))
        ((org-entry-get (point) "URL")
         (let ((url (org-entry-get (point) "URL")))
           (when url
             (cond ((string-match-p "[http\\|https]://.*?doi" url)
                    (cons 'doi (string-join (last (split-string url "/") 2) "/")))
                   ((string-match-p "https://arxiv.org" url)
                    (cons 'arxiv (ref-man-url-to-arxiv-id url)))
                   ((string-match-p "aclweb.org\\|aclanthology.info" url)
                    (cons 'acl
                          (replace-regexp-in-string
                           "\\.pdf$" ""
                           (car (last (split-string (string-remove-suffix "/" url) "/"))))))
                   ((string-match-p "semanticscholar.org" url)
                    (cons 'ss (-last-item (split-string (string-remove-suffix "/" url) "/"))))
                   (t nil)))))
        (t nil)))

(defun ref-man--insert-refs-from-seq (data name seqtype &optional ignore-errors)
  "Insert references from a given sequence at cursor.
DATA is json-data from semantic scholar.  NAME is the org heading
that will be generated.

After the heading is generated, each element of the data is
inserted as a reference.  The function to format the org entry is
determined by SEQTYPE; it can be one of 'ss 'assoc 'plist 'bibtex.

With optional IGNORE-ERRORS non-nil, ignore any errors that may
happen while inserting references in the buffer."
  (unless (eq major-mode 'org-mode)
    (message "[ref-man] Can only insert to an org buffer"))
  (let ((write-func (cond ((eq seqtype 'ss)
                           #'ref-man--org-bibtex-write-ref-from-ss-ref)
                          ((eq seqtype 'assoc)
                           #'ref-man--org-bibtex-write-ref-from-assoc)
                          ((eq seqtype 'plist)
                           #'ref-man--org-bibtex-write-ref-from-plist)
                          ((eq seqtype 'bibtex)
                           #'ref-man--org-bibtex-write-heading-from-bibtex)
                          (t nil))))
    ;; (seq-do (lambda (ref)
    ;;           (funcall write-func ref ignore-errors))
    ;;         data)
    ;; (org-insert-heading-respect-content)
    ;; (org-demote)
    ;; (outline-up-heading 1)
    ;; (forward-line)
    ;; (kill-line)
    ;; (delete-blank-lines)
    ;; (outline-previous-heading)

    (if (not write-func)
        (message (format "[ref-man] Illegal seqtype %s" seqtype))
      (when name
        (insert name))
      (org-insert-heading-respect-content)
      (org-demote)
      (seq-do (lambda (ref)
                (funcall write-func ref ignore-errors))
              data)
      (outline-up-heading 1)
      (forward-line)
      (kill-line)
      (delete-blank-lines)
      (outline-previous-heading))
))

(defun ref-man-insert-ss-data (ss-data &optional buf where ignore-errors)
  "Insert Semantic Scholar Data SS-DATA into an org buffer.

If optional BUF is given then that is the target buffer, else
defaults to CURRENT-BUFFER.  Optional WHERE specifies at which
point in buffer to insert the data.  Defaults to (point).  With
optional IGNORE-ERRORS non-nil, ignore any errors that may happen
while inserting references in the buffer.

The function assumes that it is at an org heading and inserts abstract
following the property drawer for the heading and references and
citations after that."
  (unless buf (setq buf (current-buffer)))
  (unless where (setq where (with-current-buffer buf (point))))
  ;; abstract should just be inserted as text
  (let ((abs (cdass 'abstract ss-data)))
    (when abs (ref-man-org-insert-abstract abs buf)))
  (org-insert-heading-respect-content)
  (org-demote)                          ; demote only once
  (ref-man--insert-refs-from-seq
   (cdass 'references ss-data) "references" 'ss ignore-errors)
  (org-insert-heading-respect-content)
  (ref-man--insert-refs-from-seq
   (cdass 'citations ss-data) "citations" 'ss ignore-errors)
  (outline-up-heading 1)
  (org-hide-block-all))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END org utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START html utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: Not implemented functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END html utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;; START org commands ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO: Replace all property setters in this section with
;;       ref-man--insert-org-pdf-file-property
;; TODO: Fix redundancies in bib fetching and pdf fetching
(defun ref-man-try-fetch-bib-insert-as-org-heading (&optional fetch-pdf)
  "Fetches the bib from URL in properties of current heading and updates the heading"
  ;; NOTE: This is how it should be but for now only arxiv
  ;; (let* ((url (cdr (assoc "URL" (org-entry-properties))))
  ;;        (bib-url (ref-man--try-get-bib-according-to-source url)))
  ;;   (if bib-url
  ;;       (ref-man-eww--browse-url bib-url nil (current-buffer))
  ;;     (message (format "[ref-man] No bibtex URL in %s" url))))
  (interactive)
  (if (eq major-mode 'org-mode)
      (let* ((url (cdr (assoc "URL" (org-entry-properties))))
             (bib-buf (ref-man--try-fetch-bib-buf-for-url url))
             (bib-assoc (when bib-buf (with-current-buffer bib-buf
                                        (goto-char (point-min))
                                        (when (string= "HTTP" (thing-at-point 'word))
                                          (forward-paragraph)
                                          (search-forward "@")
                                          (backward-char))
                                        (bibtex-parse-entry)))))
        (if bib-assoc
            (ref-man-org-bibtex-convert-bib-to-property bib-assoc (current-buffer) (point) nil)
          (message (format "[ref-man] Could not parse bibtex from %s" url)))
        (when (and fetch-pdf (ref-man-url-downloadable-pdf-url-p url))
          (message (format "[ref-man] Fetching pdf for url %s" url))
          ;; CHECK: Is it possible to not fetch bib and pdf separately but simultaneously instead?
          (ref-man--fetch-from-pdf-url url)))
    (message "[ref-man] Not in org-mode")))

;; FIXME: This could be `let'
(defvar ref-man-convert-links-in-subtree-to-headings-fetch--pdfs)
(defun ref-man-convert-links-in-subtree-to-headings ()
  "Convert all the links in the subtree to headings, if the link
is from a recognized parseable host. As of now, only ArXiv"
  (interactive)
  (if (eq major-mode 'org-mode)
      (progn
        (if current-prefix-arg
            (setq ref-man-convert-links-in-subtree-to-headings-fetch--pdfs t)
          (setq ref-man-convert-links-in-subtree-to-headings-fetch--pdfs nil))
        (unless (org-at-heading-p)
          (outline-previous-heading))
        (save-restriction
          (org-narrow-to-subtree)
          (org-show-subtree)
          (org-next-link)
          (org-insert-heading)
          (org-demote)
          (while (ref-man--move-first-link-to-org-property-drawer)
            (org-next-link)
            (unless org-link--search-failed
              (org-insert-heading)
              (ref-man-try-fetch-bib-insert-as-org-heading
               ref-man-convert-links-in-subtree-to-headings-fetch--pdfs)))))
    (message "[ref-man] Not in org-mode") nil))

(defun ref-man-kill-bibtex-to-org-format ()
  "Parse a bibtex entry at point and copy as org heading."
  (interactive)
  (save-excursion
    (unless (looking-at "^@")
      (re-search-backward "^@.*?{[a-z]+.*"))
    (let ((bib-assoc (bibtex-parse-entry)))
      (with-temp-buffer
        (org-mode)
        (org-insert-heading)
        (ref-man-org-bibtex-convert-bib-to-property bib-assoc (current-buffer) (point) nil)
        (kill-new (buffer-string))
        (message "Killed entry as org heading")))))

(defun ref-man-fetch-update-ss-data-on-disk-for-entry ()
  "Force update and fetch Semantic Scholar data for org entry at point."
  (interactive)
  (ref-man-fetch-ss-data-for-entry nil nil t))

;; CHECK: Should we update more than `arxivId'?
(defun ref-man-fetch-ss-data-for-entry (&optional update display update-on-disk)
  "Try to fetch the Semantic Scholar data for org entry at point.

The data is cached on the disk and if the entry is already
present, the cached entry is fetched.  With optional argument
UPDATE-ON-DISK, force upate the data in cache.

When called interactively, the default behaviour is to fetch the
data and display in a new org buffer named \"*Semantic Scholar*\".
When called from another function a non-nil DISPLAY will do the
same.

When called interactively and with a `\\[universal-argument]' or
non-nil UPDATE, only update the org entry from the Semantic
Scholar database.  With two `\\[universal-argument]' `\\[universal-argument]',
both update the entry and display the data."
  (interactive)
  (when (called-interactively-p 'any)
    ;; CHECK: Why am I doing this let to meh?
    (let ((meh current-prefix-arg))
      (cond ((and meh (equal meh '(4)))   ; update only
             (setq update t))
            ((and meh (equal meh '(16)))  ; both update and display
             (setq update t)
             (setq display t))
            (t (setq display t)))))        ; by default only display
  (if (eq major-mode 'org-mode)
      (progn
        (unless (org-at-heading-p)
          (outline-previous-heading))
        (let* ((idtype-id (ref-man--ss-id))
               (ss-data (when idtype-id
                          (message "[ref-man] Fetching Semantic Scholar Data for %s id: %s"
                                   (car idtype-id) (cdr idtype-id))
                          (with-current-buffer
                              (url-retrieve-synchronously
                               (format "http://localhost:%s/semantic_scholar?id_type=%s&id=%s%s"
                                       ref-man-python-server-port
                                       (car idtype-id)
                                       (cdr idtype-id)
                                       (if update-on-disk "&force" ""))
                               t)       ; silent
                            (goto-char (point-min))
                            (forward-paragraph)
                            (json-read)))))
          (unless ss-data
            (message "[ref-man] Could not retrieve Semantic Scholar data for entry"))
          (when (and ss-data update)
            ;; (when (cdass 'arxivId ss-data)
            ;;   (org-entry-put (point) "ARXIVID" (cdass 'arxivId ss-data))
            ;;   (org-entry-put (point) "URL" (ref-man-url-from-arxiv-id)))
            (ref-man--org-bibtex-write-ref-from-ss-ref ss-data nil t))
          (when ss-data
            (message "[ref-man] Inserting Semantic Scholar Data")
            (org-entry-put (point) "PAPERID" (cdass 'paperId ss-data))
            ;; NOTE: The data is inserted into a new buffer named "*Semantic Scholar*"
            (when display
              (let ((buf (get-buffer-create "*Semantic Scholar*"))
                    (heading (org-get-heading nil t t t)))
                (with-current-buffer buf
                  (erase-buffer)
                  (org-mode)
                  (org-insert-heading)
                  (insert heading)
                  (forward-line)
                  (ref-man-insert-ss-data ss-data buf nil t)
                  (switch-to-buffer buf)))))))
    (message "[ref-man] Not in org-mode") nil))

(defun ref-man--update-props-from-assoc (props-alist)
  (org-insert-property-drawer)
  (seq-do (lambda (x)
            (unless (string-equal (car x) "ABSTRACT")
              (org-entry-put (point) (car x) (cdr x))))
          props-alist)
  (let ((key (ref-man-parse-bib-property-key)))
    (unless key
      (setq key (read-from-minibuffer (format "Could not parse key:\nauthor: %s\ntitle: %s\nyear: %s"
                                              (org-entry-get (point) "AUTHOR")
                                              (org-entry-get (point) "TITLE")
                                              (org-entry-get (point) "YEAR")))))
    (org-set-property "CUSTOM_ID" key))
  (when (string-empty-p (org-get-heading))
    (unless (org-at-heading-p)
      (outline-previous-heading))
    (end-of-line)
    (insert (cdass "TITLE" props-alist)))
  (when (assoc "ABSTRACT" props-alist)
    (ref-man-org-insert-abstract (cdass "ABSTRACT" props-alist) (current-buffer))))

(defun ref-man-parse-ss-search-result (result)
  (let ((retval nil))
    (push `("PAPERID". ,(cdass 'id result)) retval)
    (push `("ABSTRACT". ,(cdass 'text (cdass 'paperAbstract result))) retval)
    (push `("DOI". ,(cdass 'doi (cdass 'doiInfo result))) retval)
    (push `("URL". ,(cdass 'url (cdass 'primaryPaperLink result))) retval)
    (push `("YEAR". ,(cdass 'text (cdass 'year result))) retval)
    (push `("VENUE". ,(cdass 'text (cdass 'venue result))) retval)
    (when (assoc 'pubDate result)
      (push `("MONTH" . ,(capitalize (cdass (string-to-number
                                             (nth 2 (split-string
                                                     (cdass 'pubDate result) "-")))
                                            ref-man--num-to-months)))
            retval))
    (seq-do (lambda (x)
              (if (eq (car x) 'name)
                  (push `("JOURNAL". ,(cdr x)) retval)
                (push `(,(upcase (symbol-name (car x))). ,(cdr x)) retval)))
            (cdass 'journal result))
    (push `("AUTHOR". ,(mapconcat (lambda (x)
                                    (ref-man--build-bib-author
                                     (cdass 'name (aref x 0))))
                                  (cdass 'authors result) " and "))
          retval)
    (push `("TITLE". ,(cdass 'text (cdass 'title result))) retval)
    (push '("TYPE" . "article") retval)
    (-remove (lambda (x) (string-empty-p (cdr x))) retval)))

;; TODO: prompt by default
;;
(defun ref-man-search-semantic-scholar (search-string &optional insert-first &rest args)
  "Search Semantic Scholar for SEARCH-STRING.
ARGS should valid json, e.g., {cs_only: true, has_github: false}
corresponding to javascript semantics"
  (interactive (list (let* ((ss (if (eq major-mode 'org-mode) (org-get-heading) ""))
                            (prompt (if (string-empty-p ss)
                                        "Search String: "
                                      (format "Search String (default %s): " ss))))
                       (read-from-minibuffer prompt nil nil nil nil ss))))
  (if (string-empty-p search-string)
      (message "Empty Search String")
    (let ((meh current-prefix-arg))
      (cond ((and meh (equal meh '(4)))   ; update only
             (setq insert-first t))
            ))
    ;; TODO: fetch page from python server, should use jinja or lisp template
    (let* ((args (when args (string-join args "&")))
           (url (concat (format "http://localhost:%s/semantic_scholar_search?q=%s"
                                ref-man-python-server-port search-string)
                        (if args (concat "&" args) "")))
           (buf (if args (ref-man--post-json-synchronous url args)
                  (url-retrieve-synchronously url)))
           (result (with-current-buffer buf
                     (goto-char (point-min))
                     (forward-paragraph)
                     (json-read)))
           (results (cdass 'results result)))
      (if (> (length results) 0)
          (if insert-first
              (ref-man--update-props-from-assoc
               (ref-man-parse-ss-search-result (aref results 0)))
            results)
        (message "[ref-man] No results from Semantic Scholar")))))

(defun ref-man-import-pdf-url-to-org-buffer (&optional url web-buf org-buf pt)
"Before call should check the buffer as it can't be called if
buffer is not gscholar"
  (interactive)
  (save-excursion
    (let* ((pdf-url (or url (ref-man-web-get-previous-pdf-link (or web-buf (current-buffer)))))
           (org-buf (or org-buf
                        ref-man--org-gscholar-launch-buffer
                        (let ((org-links-file-name (file-name-nondirectory ref-man-org-links-file-path)))
                          (or (get-buffer org-links-file-name)
                              (find-file-noselect ref-man-org-links-file-path)))))
           (org-point (or pt (and ref-man--org-gscholar-launch-buffer
                                  ref-man--org-gscholar-launch-point)
                          (with-current-buffer org-buf (point)))))
      (if (or (ref-man-url-downloadable-pdf-url-p pdf-url)
              (y-or-n-p (format "%s is not a valid PDF url. Add anyway? " pdf-url)))
          (with-current-buffer org-buf
            (org-entry-put org-point "PDF_URL" pdf-url))
        (message "[ref-man] Could not insert pdf url")))))

(defun ref-man-org-set-insertion-point ()
  "Set buffer and point to current buffer and point for importing references.
Only for `org-mode'."
  (interactive)
  (if (eq major-mode 'org-mode)
      (progn (setq ref-man--org-gscholar-launch-buffer (current-buffer))
             (setq ref-man--org-gscholar-launch-point (point))
             (message "[ref-man] Set buffer and point successfully."))
    (message "[ref-man] Not org mode")))

(defun ref-man--get-org-buf-and-point (&optional org-buf org-point)
  "Get the correct org buffer and point for operations.
When optional ORG-BUF and ORG-POINT are non-nil, they are given
preference over global variables
`ref-man--org-gscholar-launch-buffer' and
`ref-man--org-gscholar-launch-point'.  The last preference is
given to the buffer with file `ref-man-org-links-file-path'."
  (let* ((org-links-file-name (file-name-nondirectory ref-man-org-links-file-path))
         (org-links-buf (if (get-buffer org-links-file-name) (get-buffer org-links-file-name)
                          (find-file-noselect ref-man-org-links-file-path))))
    (list :org-buf (or org-buf ref-man--org-gscholar-launch-buffer
                       org-links-buf)
          :org-point (or org-point
                         (and org-buf (eq org-buf ref-man--org-gscholar-launch-buffer)
                              ref-man--org-gscholar-launch-point)
                         (and org-buf (with-current-buffer org-buf (point)))
                         (and ref-man--org-gscholar-launch-buffer
                              ref-man--org-gscholar-launch-point)
                         (and org-links-buf
                              (with-current-buffer org-links-buf (point)))))))

(defun ref-man-org-import-link (args &optional current)
  "Import link to an org buffer.
ARGS are a plist constitute the data for the link.

:link LINK -- the uri of the link

:link-text LINK-TEXT -- the text of the link, inserted as title/heading

:metadata METADATA -- From Google Scholar, it's the line immediately after the title.

Optional non-nil argument CURRENT specifies whether to update the
current headline.  Default is to insert a subheading."
  (let*
      ;; ((org-buf (or ref-man--org-gscholar-launch-buffer
      ;;                 (let ((org-links-file-name (file-name-nondirectory ref-man-org-links-file-path)))
      ;;                   (if (get-buffer org-links-file-name) (get-buffer org-links-file-name)
      ;;                     (find-file-noselect ref-man-org-links-file-path)))))
      ;;    (org-point (or (and ref-man--org-gscholar-launch-buffer ref-man--org-gscholar-launch-point)
      ;;                   (with-current-buffer org-buf (point)))))
      ((org-data (ref-man--get-org-buf-and-point))
       (org-buf (plist-get org-data :org-buf))
       ;; NOTE: Not used
       ;; (org-point (plist-get org-data :org-point))
       (link (cdass 'link args))
       (title (cdass 'title args))
       (metadata (cdass 'metadata args))
       (authors (cdass 'authors args))
       (date (cdass 'date args))
       (abstract (cdass 'abstract args)))
    (unless metadata
      (setq metadata (-filter 'cdr `((authors . ,authors) (date . ,date) (abstract . ,abstract)))))
    (with-current-buffer org-buf
      (ref-man-org-insert-link-as-headline org-buf link title metadata current))))

(defun ref-man--download-pdf-redirect-new (callback url &optional args)
  (message (concat "[ref-man] Fetching PDF from " url))
  (let ((url (ref-man-url-maybe-proxy url)))
    (url-retrieve url callback (list url args))))

(defun ref-man--download-pdf-redirect (callback url &optional point)
  (message (concat "[ref-man] Fetching PDF from " url))
  (let ((url (ref-man-url-maybe-proxy url)))
    (if point
        (url-retrieve url callback (list url point))
      (url-retrieve url callback (list url)))))

(defun ref-man--fetch-from-pdf-url-new (url &optional args)
  "Fetch pdf file if possible, from URL.
Optional argument STOREP is for batch updates.  Store the
filename in `ref-man--subtree-list' instead so that the whole
subtree will be updated later."
  (if (and url (not (string-empty-p url)))
      (let ((file (ref-man-files-check-pdf-file-exists url t))
            (url (ref-man-url-maybe-proxy url))
            (buf (and args (plist-get args :buffer)))
            (pt (and args (plist-get args :point)))
            (heading (and args (plist-get args :heading))))
        (cond ((and file (with-current-buffer buf (eq major-mode 'org-mode)))
               (message "[ref-man] File already existed.")
               (if buf
                   (with-current-buffer buf
                     (cond ((string-empty-p (string-trim (replace-regexp-in-string "\*" "" heading)))
                            (goto-char pt)
                            (ref-man--insert-org-pdf-file-property file))
                           (t (save-excursion
                                (org-link-search heading)
                                (ref-man--insert-org-pdf-file-property file)))))
                 (with-current-buffer ref-man--org-gscholar-launch-buffer
                   (save-excursion
                     (goto-char ref-man--org-gscholar-launch-point)
                     (ref-man--insert-org-pdf-file-property file)))))
              ((and (not file) args)
               (ref-man--download-pdf-redirect-new
                #'ref-man--eww-pdf-download-callback-store-new url args))
              ((and (not file) (not args))
               (save-excursion
                 (with-current-buffer ref-man--org-gscholar-launch-buffer
                   (goto-char ref-man--org-gscholar-launch-point)
                   (ref-man--insert-org-pdf-file-property file))))))
    (message "[ref-man] Empty pdf url given")))

(defun ref-man--fetch-from-pdf-url (url &optional storep)
  "Fetch pdf file if possible, from URL.
Optional argument STOREP is for batch updates.  Store the
filename in `ref-man--subtree-list' instead so that the whole
subtree will be updated later."
  (let ((file (ref-man-files-check-pdf-file-exists url t))
        (url (ref-man-url-maybe-proxy url)))
    (cond ((and file (not storep))
           (message "[ref-man] File already existed.")
           ;; FIXME: What if it inserts to wrong buffer?
           (if (eq major-mode 'org-mode)
               (ref-man--insert-org-pdf-file-property file)
             (with-current-buffer ref-man--org-gscholar-launch-buffer
               (goto-char ref-man--org-gscholar-launch-point)
               (ref-man--insert-org-pdf-file-property file))))
          ((and file storep)
           (let* ((elem (-first (lambda (x) (string= (plist-get x :url) url)) ref-man--subtree-list))
                  (repl (plist-put elem :file file)))
             (setq ref-man--subtree-list (-replace-first elem repl ref-man--subtree-list)))
           ;; (setq ref-man--subtree-list (plist-put ref-man--subtree-list (point) file))
           ;; FIXME: What if it inserts to wrong buffer?
           (ref-man--insert-org-pdf-file-property file))
          ;; CHECK: Have to find a better way than to store (point) maybe
          ((and (not file) storep)
           (ref-man--download-pdf-redirect #'ref-man--eww-pdf-download-callback-store url (point)))
          ((and (not file) (not storep))
           (ref-man--download-pdf-redirect #'ref-man--eww-pdf-download-callback url)))))
(make-obsolete 'ref-man--fetch-from-pdf-url 'ref-man--fetch-from-pdf-url-new "ref-man 0.3.0")

(defun ref-man--update-subtree-list (url status)
  (push (list :url url :point (point)
              :heading (org-link-heading-search-string)
              :status status)
        ref-man--subtree-list))

;; CHECK: Curious thing is, pdf retrieval is async and bib retrieval isn't I
;;        should make it uniform
;; NOTE: Adding update heading also if heading is null
;; NOTE: This is only called by `ref-man-try-fetch-and-store-pdf-in-org-entry'
;;
;; FIXME: Can this bet `let'
(defvar ref-man--fetched-url-title)
(defvar ref-man--fetched-url-buffers nil
  "Internal variable to hold fetched (url . buffer) pairs.")
(defvar ref-man--fetching-url-buffers nil
  "Internal variable to hold (url . buffer) pairs being fetched right now.")

;; FIXME: This should not really depend on the mode and the buffer should be
;;        sent from the interactive function
;; TODO: What if the buffer gets killed before the pdf/bib is fetched?
(defun ref-man-try-fetch-pdf-from-url (org-buf pt heading url pdf-url
                                               &optional retrieve-pdf retrieve-bib
                                                         retrieve-title storep)
  "Try and fetch pdf and bib entry from URL.

Can only retrieve from specific urls.  Optional STOREP specifies
whether to store the retrieved data to org file or not.

If at least one of two optional arguments RETRIEVE-PDF or
RETRIEVE-BIB are non-nil then the corresponding pdf and/or bibtex
entry are/is fetched and stored if the option is given.  Org
buffer to insert the data is set by
`ref-man--org-gscholar-launch-buffer'.

RETRIEVE-TITLE has no effect at the moment."

  ;; NOTE: Here I'll have to write rules to fetch pdfs from different urls,
  ;;       e.g. arxiv.org, nips, cvpr, iccv, aaai, tacl, aclweb, pmlr (jmlr,
  ;;       icml) Should it just return a downloadable link or download the file
  ;;       itself?

  ;; NOTE: I've commented the following out here as I'm trying to reduce
  ;;       dependence on global variables. These should only be used when
  ;;       calling from non org modes.
  ;;
  ;; (if (eq major-mode 'org-mode)
  ;;     (progn
  ;;       (setq ref-man--org-gscholar-launch-buffer (current-buffer))
  ;;       (setq ref-man--org-gscholar-launch-point (point)))
  ;;   (setq ref-man--org-gscholar-launch-buffer nil)
  ;;   (setq ref-man--org-gscholar-launch-point nil)) ; needn't be at heading

  ;; NOTE: prefetch url
  ;;
  ;; (when url
  ;;   (let ((-url (ref-man-url-maybe-unproxy url)))
  ;;     (unless (member -url ref-man--fetching-url-buffers)
  ;;       (push -url ref-man--fetching-url-buffers))
  ;;     (setq ref-man--fetched-url-buffers
  ;;           (-remove-first (lambda (x) (equal (car x) -url))
  ;;                          ref-man--fetched-url-buffers))
  ;;     (url-retrieve -url (lambda (status -url)
  ;;                          (push (cons -url (buffer-name (current-buffer)))
  ;;                                ref-man--fetched-url-buffers))
  ;;                   (list -url))))
  (if (eq major-mode 'org-mode)
      (progn
        (when retrieve-pdf
          ;; TEMP
          ;;
          ;; NOTE: I had written a subroutine for checking the file if it's a pdf but
          ;;       it required opening the buffer in raw mode I think, which is not
          ;;       ideal. Either have a shell command built in or use python or
          ;;       something. python in most cases may be more platform independent.
          ;;
          ;; NOTE: Initial code for method to solve current issues
          ;;       1. pdf-url may succeed
          ;;       2. url and bib from url and pdf-url from url should not lead to
          ;;          url being fetched multiple times
          ;;       3. fetch-bib and fetch-pdf should happen in parallel
          ;; Problem is that pdf download is with callback
          ;; (defun ref-man-download-and-check (url-or-buffer)
          ;;   )
          ;; (let* ((downloaded (when (and pdf-url (ref-man-url-downloadable-pdf-url-p pdf-url))
          ;;                      (ref-man-download-and-check pdf-url)))
          ;;        (downloaded (if downloaded downloaded
          ;;                      ;; Wait for buffer in `ref-man--fetched-url-buffers'
          ;;                      (when (ref-man-url-downloadable-pdf-url-p url)
          ;;                        (ref-man-download-and-check url))))
          ;;        (downloaded (if downladed downloaded
          ;;                      (ref-man-download-and-check
          ;;                       (ref-man-url-get-pdf-url-according-to-source
          ;;                        (cdass url ref-man--fetched-url-buffers))))))
          ;;   downloaded)
          ;; END TEMP
          (let (maybe-pdf-url)
            (setq maybe-pdf-url (cond (pdf-url
                                       (ref-man--fetch-from-pdf-url-new
                                        pdf-url (list :buffer org-buf
                                                      :point pt
                                                      :heading heading))
                                       pdf-url)
                                      ((and url (ref-man-url-downloadable-pdf-url-p url))
                                       (ref-man--fetch-from-pdf-url-new
                                        url (list :buffer org-buf :point pt
                                                  :heading heading))
                                       url)
                                      (t (ref-man-url-get-pdf-url-according-to-source
                                          url #'ref-man--fetch-from-pdf-url-new
                                          (list :buffer org-buf :point pt
                                                :heading heading))
                                         url)))
            ;; FIXME: Subtree list needn't exist. Pass org heading (and point)
            ;;        to fetch-from-pdf-url callback
            (when storep
              (ref-man--update-subtree-list maybe-pdf-url "fetching")))
          ;; (let ((maybe-pdf-url (or pdf-url (if (ref-man-url-downloadable-pdf-url-p url) url
          ;;                                    (ref-man-url-get-pdf-url-according-to-source url)))))
          ;;   (cond (maybe-pdf-url
          ;;          (when storep
          ;;            (push (list :url maybe-pdf-url :point (point)
          ;;                        :heading (org-link-heading-search-string)
          ;;                        :status "fetching")
          ;;                  ref-man--subtree-list))
          ;;          (ref-man--fetch-from-pdf-url maybe-pdf-url storep))
          ;;         ((not maybe-pdf-url)
          ;;          (if storep
          ;;              (push (list :url maybe-pdf-url :point (point)
          ;;                          :heading (org-link-heading-search-string)
          ;;                          :status "NOT_PDF")
          ;;                    ref-man--subtree-list)
          ;;            (message (concat "[ref-man] Browsing " url))
          ;;            (eww-browse-url url)))
          ;;         ((not url)
          ;;          (if storep
          ;;              (push (list :url url :point (point)
          ;;                          :heading (org-link-heading-search-string)
          ;;                          :status "BAD_URL")
          ;;                    ref-man--subtree-list)
          ;;            (message (concat "[ref-man] Bad URL " url))))
          ;;         (t (message "[ref-man--try-fetch-pdf-from-url] Some strange error occured. Check"))))
          )
        (when (and retrieve-bib (not storep)) ; don't try retrieve bib when storep
          (message "[ref-man] NOT Retrieving bib entry")
          ;; (message "[ref-man] Retrieving bib entry")
          ;; (cond ((and url (ref-man-url-has-bib-url-p url))
          ;;        (let ((bib-url (ref-man--try-get-bib-according-to-source url)))
          ;;          (if bib-url
          ;;              (ref-man-eww--browse-url bib-url nil ref-man--org-gscholar-launch-buffer)
          ;;            (message (format "[ref-man] No bibtex URL in %s" url)))))
          ;;       ((not url)
          ;;        (message "[ref-man] No URL given"))
          ;;       ((and url (not (ref-man-url-has-bib-url-p url)))
          ;;        (message "[ref-man] URL doesn't have a bib URL"))
          ;;       (t (message (format "[ref-man] Some strange error occured for retrieve-bib in %s. Check" url))))
          )
        (when retrieve-title
          ;; NOTE: Not worth it right now.
          (message "[ref-man] Let's see if we can insert heading"))
        (when (and (not retrieve-bib) (not retrieve-pdf))
          (message "[ref-man] Nothing to do")))
    (message "[ref-man] Not in org mode")))

;; FIXME: Although it works in principle, but for a large subtree it sort of
;;        hangs and the multithreading is not really that great in emacs.
;;
;; TODO: How to make a region read only of the org file and then replace the
;;       contents in there later
;;       (defun set-region-read-only (begin end)
;;         (interactive "r")
;;        (add-text-properties begin end '(read-only t)))
;;        https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-Properties.html#Text-Properties
;;        https://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Properties.html#Special-Properties

(defun ref-man-try-fetch-and-store-pdf-in-org-subtree-entries ()
  (interactive)
  (message "[ref-man] Fetching PDFs in subtree. Wait for completion...")
  (setq ref-man--current-org-buffer (current-buffer))
  (setq ref-man--subtree-list nil)
  (save-restriction
    (org-narrow-to-subtree)
    (org-content t)
    (setq ref-man--subtree-num-entries 0)
    (while (not (eobp))
      (when (outline-next-heading)
        (cl-incf ref-man--subtree-num-entries)
        (ref-man-try-fetch-and-store-pdf-in-org-entry t)))))

(defun ref-man-convert-links-to-headings-in-subtree ()
  "Convert all links in the body of the current heading to a heading.
The links are assumed to be PDF/publication links and the
properties are fetched as such.  Each link is then inserted one
level deeper from the current heading as a new entry."
  (interactive)
  (message "[ref-man] Converting links to headings...")
  (if (eq major-mode 'org-mode)
      (let ((links (ref-man-org-get-links-of-type (current-buffer) "https" t)))
        (save-restriction
          (org-narrow-to-subtree)
          (let ((level (ref-man-org-heading-level))
                (offset 0))
            (save-excursion
              (seq-do (lambda (link) (let* ((beg (+ (org-element-property :begin link) offset))
                                            (end (+ (org-element-property :end link) offset))
                                            (text (org-element-property :raw-link link)))
                                       (delete-region beg end)
                                       (setq offset (+ offset (- beg end)))
                                       (org-insert-heading-after-current)
                                       (when (= level (ref-man-org-heading-level (point)))
                                         (org-demote-subtree))
                                       (insert text)
                                       (forward-line)
                                       (org-indent-line)
                                       (insert text)))
                      links)
              (ref-man-delete-blank-lines-in-buffer)))
          ))
    (message "[ref-man] Not in org mode")))

(defun ref-man-org-narrow-to-heading-and-body ()
  "Narrow to the current heading and the body.
Unlike `org-narrow-to-subtree' any headings which are children of
the current heading are excluded."
  (let (ref-man--point-min
        ref-man--point-max)
    (org-narrow-to-subtree)
    (save-excursion
      (org-show-subtree)
      (beginning-of-line)
      (if (eq (point-min) (point))
          (setq ref-man--point-min (point))
        (org-previous-visible-heading 1)
        (setq ref-man--point-min (point)))
      (org-next-visible-heading 1)
      (setq ref-man--point-max (point))
      (narrow-to-region ref-man--point-min ref-man--point-max))))

(defun ref-man--check-heading-p ()
  (> (length (string-trim (substring-no-properties (org-get-heading t t)))) 0))

(defun ref-man--check-fix-url-property ()
  "Fix the URL property in the property drawer.
Make sure URL property exists in either property drawer or text
and if no URL could be found return nil.  If no URL property
exists, then first link from entry text is imported into the
property drawer as the URL property."
  (save-excursion
    (let* ((props (org-entry-properties))
           (url-prop (cdr (assoc "URL" props))))
      ;; Remove the url args. Would be useless to keep
      (when (and url-prop (string-match-p "?.*" url-prop)
                 (not (string-match-p "openreview.net" url-prop))
                 (not (string-match-p "acm.org" url-prop)))
        (org-entry-put (point) "URL" (car (split-string url-prop "?"))))
      (unless url-prop
        (let* ((link (ref-man-org-get-first-link-from-org-heading))
               (url (org-element-property :raw-link link))
               (beg (org-element-property :begin link))
               (end (org-element-property :end link)))
          (when url
            (delete-region beg end)
            (goto-char beg)
            (cond ((and (org-at-item-p)
                        (not (string-match-p
                              "[^- ]" (buffer-substring-no-properties (point-at-bol)
                                                                      (point-at-eol)))))
                   (delete-region (point-at-bol) (+ 1 (point-at-eol))))
                  ((and (not (string-match-p
                              "[^[:space:]]" (buffer-substring-no-properties (point-at-bol)
                                                                             (point-at-eol)))))
                   (delete-region (point-at-bol) (+ 1 (point-at-eol)))))
            (outline-previous-heading)
            (org-entry-put (point) "URL"  url))
          (setq url-prop url)))
      url-prop)))

(defun ref-man-parse-bib-property-key ()
  "Check if bibtex key can be determined from entry properties."
  (interactive)
  (let* ((props (org-entry-properties))
         (title (cdr (assoc "TITLE" props)))
         (author (cdr (assoc "AUTHOR" props)))
         (year (cdr (assoc "YEAR" props))))
    (when (and title author year)
      (let ((key (ref-man--build-bib-key-from-plist
                  (list :title title :author author :year year))))
        (if (called-interactively-p 'any)
            (kill-new key)
          key)))))

(defun ref-man--check-fix-pdf-file-property ()
  (let* ((props (org-entry-properties))
         (pdf-file (cond ((assoc "PDF_FILE" props)
                          (cdr (assoc "PDF_FILE" props)))
                         ((assoc "FILE_NAME" props)
                          (ref-man--insert-org-pdf-file-property
                           (replace-regexp-in-string "\\[\\|\\]" "" (cdr (assoc "FILE_NAME" props)))))
                         (t nil))))
    (when (assoc "FILE_NAME" props)
      (org-delete-property "FILE_NAME"))
    pdf-file))

;; TODO: Link (arxiv, ACL, DOI) to SS_IDs for papers also, minimize redundancy
;;       NOTE: Perhaps maintain a python cache for that
;; TODO: I have not incorporated dblp extracted entries as they refer
;;       to DOIs or direct arxiv links. Have to fix that.
;; TODO: If the pdf is extracted from a site like cvf, or nips
;;       or whatever, the corresponding bibtex should also be
;;       extracted and stored from the website itself.
;; TODO: What if entry exists but the file doesn't?
;; TODO: What if the buffer gets killed before the pdf/bib is fetched?
(defun ref-man-try-fetch-and-store-pdf-in-org-entry (&optional storep)
  "Try to fetch and store pdf for current entry in an org buffer."
  (interactive)
  (org-insert-property-drawer)
  (if (eq major-mode 'org-mode)
      (let* ((buf (current-buffer))
             (pt (point))
             (heading (org-link-heading-search-string))
             (props (org-entry-properties))
             (url-prop (ref-man--check-fix-url-property))
             ;; CHECK: Why's this not used?
             (pdf-url-prop (cdass "PDF_URL" props))
             (ssidtype-id (ref-man--ss-id))
             (pdf-file (ref-man--check-fix-pdf-file-property))
             (bib-prop (ref-man-parse-bib-property-key))
             (headingp (ref-man--check-heading-p))
             retrieve-bib retrieve-pdf retrieve-title
             (msg-str ""))
        (when ssidtype-id
          ;; TODO: if ssidtype-id then check if SS has paper
          )
        ;; TODO: Use helm instead of prefixes
        ;;       Actually Should use `hydra' but that should be for everything
        ;; FIXME: Make sure pdf file exists on disk, else remove prop
        ;; FIXME: Maybe check for :link property from text but MEH! NOTE: Not sure about this
        (let ((meh current-prefix-arg))
          (cond ((and meh (equal meh '(4)))   ; update only
                 ;; Do something
                 ))
          )
        ;; CHECK: storep signifies to store the filename in PDF_FILE property?
        (setq msg-str
              (concat msg-str
                      (cond ((and pdf-file (or pdf-url-prop url-prop))
                             (concat "[ref-man] PDF already exists."
                                     (if (string= (replace-regexp-in-string "\\[\\|\\]" "" pdf-file)
                                                  (ref-man-files-filename-from-url (or pdf-url-prop url-prop)))
                                         " And is the same as URL"
                                       (format " But is different from URL [%s]"
                                               (ref-man-files-filename-from-url (or pdf-url-prop url-prop))))))
                            ((and pdf-file (not (or pdf-url-prop url-prop)))
                             "[ref-man] PDF exists, but no URL! What to do?!!")
                            ((and (not pdf-file) (or pdf-url-prop url-prop))
                             (setq retrieve-pdf t)
                             "[ref-man] No pdf file in properties. Will fetch PDF from URL")
                            ((and (not pdf-file) (not (or pdf-url-prop url-prop)))
                             "[ref-man] Nothing to be done here for pdf file"))
                      "\n[ref-man] "
                      ;; NOTE: Check for bibtex also
                      (cond ((not bib-prop)
                             (setq retrieve-bib t)
                             "No bib in properties. Will fetch from URL")
                            ((and bib-prop (not (cdr (assoc "CUSTOM_ID" props))))
                             (org-set-property "CUSTOM_ID" bib-prop)
                             "Bib in properties, but no CUSTOM_ID. Fixed!")
                            ((and bib-prop (string= bib-prop (cdr (assoc "CUSTOM_ID" props))))
                             "Bib in properties, and same as key!")
                            ((and bib-prop (not (string= bib-prop (cdr (assoc "CUSTOM_ID" props)))))
                             (format "Bib in properties, but different from key {%s}. Check!!" bib-prop)))
                      "\n[ref-man] "
                      (cond ((and headingp url-prop)
                             (setq retrieve-title nil)
                             "Heading exists!")
                            ((and (not headingp) url-prop)
                             (setq retrieve-title t)
                             "No heading. Will fetch from URL!")
                            ((and (not headingp) (not url-prop))
                             (setq retrieve-title nil)
                             "No heading and no URL. What to do?!!"))))
        (unless storep
          (message msg-str))
        (when (or retrieve-pdf retrieve-bib)
          (ref-man-try-fetch-pdf-from-url buf pt heading url-prop pdf-url-prop retrieve-pdf
                                          retrieve-bib retrieve-title storep)))
    (message "[ref-man] Not in org-mode") nil))

;; FIXED: Since the retrieval is now async, if I move to another
;;        headline while the file is being downloaded, the file link
;;        may get inserted in the other headline.
;;        Will need to store some list for the corresponding points
;;        with push and pop function so that the correct property is
;;        inserted to each headline.
;; TODO: Even though the above issue is fixed, I'm now confronted with
;;       the issue that if I call the function on two org entries, the
;;       global variable will be over written and there's no way to
;;       avoid that except by using assoc lists and mutexes.
;; (defun ref-man-try-fetch-and-store-pdf-in-org-entry-text (&optional storep)
;;   "For the first link in text, ascertain if the corresponding pdf
;; file exists or not. In case it doesn't, retrieve the pdf file
;; from the url if it is a downloadable url or an arxiv url. If the
;; link is not a pdf link, call eww-browse-url. If the file exists,
;; don't retrieve the pdf file.
;; In both cases, store the file link in PDF_FILE property in the org
;; property drawer for the headline at point."
;;   (let* ((props (text-properties-at (point)))
;;          (url (car (mapcar (lambda (x) (cadr x))
;;                            (-filter #'ref-man--org-property-is-url-p props))))
;;          (url (if url url (org-element-property :raw-link
;;                                                 (ref-man-org-get-first-link-from-org-heading)))))
;;     (when url
;;       (cond ((= 1 (ref-man-org-get-number-of-http-links-from-org-buffer))
;;              (ref-man--move-only-link-to-org-property-drawer))
;;             ((< 1 (ref-man-org-get-number-of-http-links-from-org-buffer))
;;              (y-or-n-p "Move first link to property? ")
;;              (let* ((link (ref-man-org-get-first-link-from-org-heading))
;;                     (link-str (org-element-property :raw-link link)))
;;                (delete-region (org-element-property :begin link)
;;                               (org-element-property :end link))
;;                (org-set-property "URL" link-str))))
;;       (ref-man-try-fetch-pdf-from-url url storep))))

(defun ref-man-eww-download-pdf (url &optional view)
  "Download the pdf file from a website and optionally view it"
  (interactive)
  (let ((file (ref-man-files-check-pdf-file-exists url t))
        (url (ref-man-url-maybe-proxy url)))
    (if file
        (if (y-or-n-p "File exists.  Download again? ")
            (progn (message (format "Downloading %s" url))
                   (url-retrieve url #'ref-man--eww-pdf-download-callback (list url view t))) ; retrieve and view
          (when view (find-file-other-window file))) ; else view
      (message (format "Downloading %s" url))
      (url-retrieve url #'ref-man--eww-pdf-download-callback (list url view)))))

;; called from an org buffer
;; when to set ref-man--org-gscholar-launch-buffer nil?
;;
;; when gscholar is called from any other buffer than previous
;; ref-man--org-gscholar-launch-point
(defun ref-man-org-search-heading-on-gscholar-with-eww ()
  "Searches for the current heading in google scholar in
eww. Stores the buffer and the position from where it was called."
  (interactive)
  (if (eq major-mode 'org-mode)
      (progn
        (setq ref-man--org-gscholar-launch-point (point)) ; must be at heading?
        (setq ref-man--org-gscholar-launch-buffer (current-buffer))
        (save-excursion
          (let ((query-string (replace-regexp-in-string ":\\|/" " "
                               (substring-no-properties (org-get-heading t t)))))
            (ref-man-web-gscholar query-string))))
    (message "[ref-man] Not in org-mode") nil))
(make-obsolete 'ref-man-org-search-heading-on-gscholar-with-eww
               'ref-man-search-web "")
;; FIXME: This function may take up a lot of processing. Should implement some
;;        cache for it Can be called after downloading pdf from any website
;;
;; FIXME: This function is referenced at some places but is commented
;;        out. Should check if I haven't implemented alternate versions
;;        DEPRECATED (possibly)
;;
;; NOTE: Actually I had wanted to import everything automatically from
;;       eww. Heading if it existed, or create one. Check for files etc. I guess
;;       I can do it later
(defun ref-man-maybe-create-or-insert-org-heading-and-property (file)
  "For a given filename, a heading and/or :PDF_FILE: property is
inserted in ref-man--org-gscholar-launch-buffer if it exists, or
temp-org-buffer.
a) org-gscholar-launch-buffer if it exists
b) temp-org-buffer otherwise

If the pdf already exists in the download directory
a) org-gscholar-launch-buffer is checked for the entry"
  ;; - If org-gscholar-launch-buffer exists
  ;;   When does it exist though? What if I go to eww from org with eww?
  ;;   - Save excursion, get pdf file name and search in the buffer
  ;;   - If it matches in any link [[][]] in heading then insert file link for the org heading
  ;;     - Else if *eww* buffer was gscholar or arxiv
  ;;       generate org-heading-after-checking if it exists
  ;;     - Else generate org-heading-from-title
  ;; - Else Repeat with temp-org-buffer instead
  (let* ((buf (if ref-man--org-gscholar-launch-buffer ref-man--org-gscholar-launch-buffer
                (get-buffer-create ref-man-org-links-file-path)))
         (file (ref-man-files-check-pdf-file-exists file)))
    ;; Where is the corresponding headline?
    ;; Check if eww-import-link exists instead
    (cond ((and buf file)
           (with-current-buffer buf
             (when ref-man--eww-import-link
               (org-set-property "URL" ref-man--eww-import-link))
             (with-current-buffer ref-man--org-gscholar-launch-buffer
               (save-excursion
                 (goto-char ref-man--org-gscholar-launch-point)
                 (ref-man--insert-org-pdf-file-property file))))))))

(defun ref-man--at-bib-heading-p ()
  (let ((props (org-entry-properties)))
    (and (org-at-heading-p) (assoc "BTYPE" props) (assoc "CUSTOM_ID" props) t)))


;; FIXME: This function looks a bit redundant. It searches on gscholar but I've
;;        incorporated additional sources. It's sort of a last resort right now
;;
;; CHECK: Not sure why the above fixme is there, as this function is being used
;;        as a subroutine in other functions
;; FIXME: Need to remove current property drawer before updating
(defun ref-man-parse-bibtex (source-buf &optional org pt bib url kill)
  "Extract a bibtex entry from a bibtex buffer SOURCE-BUF.

With optional argument ORG, extract to open org buffer ORG else
`ref-man--org-gscholar-launch-buffer' if ORG is nil.  PT is the
point in the org buffer where the operation is to be performed.
It defaults to `point' in the ORG buffer if nil.  If URL is
non-nil, also update the URL property of the org heading at point
in the org buffer.

If optional argument BIB is non-nil, also extract to open bibtex
buffer BIB.  If optional argument KILL is non-nil, add the bibtex
entry to kill ring."
  ;; NOTE: Sanitize org entry and insert into org buffer
  (let* ((org-buf (cond ((bufferp org) org)
                        ((stringp org) (get-buffer org))
                        (ref-man--org-gscholar-launch-buffer ref-man--org-gscholar-launch-buffer)
                        (t nil)))
         (bib-buf (cond ((bufferp bib) bib)
                        ((stringp bib) (get-buffer bib))
                        ((not bib) nil)))
         (pt (or pt (and ref-man--org-gscholar-launch-buffer
                         ref-man--org-gscholar-launch-point)
                 (and org-buf (with-current-buffer org-buf (point)))))
         (kill (or kill (not (or org bib))))
         (buf-string (with-current-buffer source-buf (buffer-string))))
    (when org-buf
      (progn
        (message (concat "[ref-man] Trying to insert into org buffer: "
                         (buffer-name org-buf)))
        ;; NOTE: Sanitize entry *ONLY* at point
        (ref-man--sanitize-org-entry org-buf pt)
        (let ((bib-alist (with-current-buffer source-buf
                           (goto-char (point-min))
                           (when (string= "HTTP" (thing-at-point 'word))
                             (forward-paragraph)
                             (search-forward "@")
                             (backward-char))
                           (bibtex-parse-entry)))
              (current-key (with-current-buffer org-buf
                             (goto-char pt)
                             (org-entry-get (point) "CUSTOM_ID"))))
          (kill-buffer source-buf)
          (when url
            (push (cons "url" url) bib-alist))
          (cond ((or (not current-key) (string-match-p "na_" current-key))
                 (goto-char pt)
                 (ref-man-org-clear-property-drawer)
                 (ref-man-org-bibtex-convert-bib-to-property
                  bib-alist org-buf pt t))
                ((y-or-n-p "Authoritative entry.  Really replace? ")
                 (ref-man-org-clear-property-drawer)
                 (ref-man-org-bibtex-convert-bib-to-property
                  bib-alist org-buf pt t))))))
    (when bib-buf
      (with-current-buffer bib-buf (goto-char (point-min))
                           (insert buf-string))
      (message (format "[ref-man] inserted bib entry into %s" bib-buf)))
    ;; (when bib-buf
    ;;   (let* ((temp-bib-file-name (file-name-nondirectory ref-man-temp-bib-file-path))
    ;;          (buf (if (get-buffer temp-bib-file-name) (get-buffer temp-bib-file-name)
    ;;                 (find-file-noselect ref-man-temp-bib-file-path))))
    ;;     (with-current-buffer buf (goto-char (point-min))
    ;;                          (insert buf-string))
    ;;     (message (concat "[ref-man] inserted bib entry into " temp-bib-file-name))))
    (when kill
      (kill-new buf-string))))

;; NOTE: I wanted to do recursion also with this. Maybe use helper
(defun ref-man-parse-subtree-to-buffer-as-bibtex (&optional bib-buf)
  "Inserts contents of an org-subtree as bibtex entries to a bib file"
  (interactive)
  (unless (boundp 'bib-buf)
    (completing-read "Bib buffer: "
                     (mapcar (lambda (x) (format "%s" x)) (buffer-list))))
  (if (org-at-heading-p)
      (let (heading-has-children)
        (when heading-has-children

          ;; Import heading to bib buffer
          ;; Descend into subtree
          ;; On all children do recursive call to self
          ))
    (message "[ref-man] Not at heading"))                             ; HERE
  (when (ref-man--at-bib-heading-p)
    (ref-man-parse-subtree-to-buffer-as-bibtex bib-buf))
  (if (not bib-buf)
      (progn (message "[ref-man] bib-buf is required when called non-interactively") nil)
    (with-current-buffer (get-buffer bib-buf)
      (goto-char (point-min))
      ;; Insert stuff
      )))
;;;;;;;;;;;;;;;;;;;;;;
;; END org commands ;;
;;;;;;;;;;;;;;;;;;;;;;

(provide 'ref-man-core)

;;; ref-man-core.el ends here
