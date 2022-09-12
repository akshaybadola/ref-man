;;; ref-man-core.el --- Core Components for `ref-man'. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019,2020,2021,2022
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Monday 12 September 2022 08:52:52 AM IST>
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
;; `org', `bibtex', `science-parse' and `python'.  A python flask server
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

(require 'a)
(require 'async)
(require 'biblio-core)
(require 'bibtex)   ; Primary function I use from 'bibtex is 'bibtex-parse-entry
(require 'bind-key)
(require 'cl-lib)
(require 'citeproc)
(require 'dash)
(require 'eww)
(require 'f)
(require 'gscholar-bibtex)              ; NOTE: Maybe remove this eventually
(require 'json)
(require 'org)
(require 'org-element)
(require 'ov)
(require 'pos-tip)
(require 'seq)
(require 'shr)
(require 'subr-x)
(require 'thingatpt)
(require 'time-stamp)
(require 'url)
(require 'xml)
(require 'util)
(require 'util/org "util-org")

(require 'ref-man-util)
(require 'ref-man-files)
(require 'ref-man-url)
(require 'ref-man-web)
(require 'ref-man-py)
(require 'ref-man-ss)

(defgroup ref-man nil
  "Bibliography Manager."
  :prefix "ref-man-"
  :group 'ref-man)

(defcustom ref-man-data-root-dir (expand-file-name "~/.ref-man")
  "Root directory where ref-man data is stored."
  :type 'directory
  :group 'ref-man)

(defcustom ref-man-org-links-file-path (expand-file-name "~/.ref-man/.temp-org-links.org")
  "Temporary org file to hold URLs and metadata."
  :type 'file
  :group 'ref-man)

(defcustom ref-man-bib-files nil
  "List of `bibtex' files to search for references while generating articles."
  :type '(repeat string)
  :group 'ref-man)

(defcustom ref-man-temp-bib-file-path (expand-file-name "~/.ref-man/.temp.bib")
  "Temporary bib file to append any extract bibtex info."
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

(defcustom ref-man-pandoc-executable "/usr/bin/pandoc"
  "`pandoc' executable to use."
  :type 'file
  :group 'ref-man)

(defcustom ref-man-always-update-heading-if-different nil
  "Always update entry heading if different from fetched data."
  :type 'boolean
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
(defvar ref-man-py-data-dir)        ; from `ref-man-py'
(defvar ref-man-py-server-port)      ; from `ref-man-py'
(defvar ref-man-public-links-cache)      ; from `ref-man-remote'
(defvar ref-man-public-links-cache-file) ; from `ref-man-remote'
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
(defvar ref-man--subtree-num-entries nil)
(defvar ref-man-org-file-link-re "\\[\\(?:\\[\\(.+?\\)]\\)?\\[\\(.+?\\)]]")

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

(defun ref-man-pandoc-version ()
  "The version of pandoc being used."
  (cadr (split-string
         (shell-command-to-string
          (format "%s --version" ref-man-pandoc-executable)))))

;;
;; Constants. perhaps can name them better
;; Also should be shifted to defcustom
;;
(defvar ref-man-venue-priorities
  (let* ((confs '("icml" "nips" "iccv" "cvpr" "ijcai" "aaai" "eccv"))
         (confs-seq (number-sequence (length confs) 1 -1)))
    (cl-mapcar 'cons confs confs-seq))
  "Venue priority list from high to low.")

(defvar ref-man-venues
  '(("nips" . "Advances in Neural Information Processing Systems")
    ("neurips" . "Advances in Neural Information Processing Systems")
    ("iccv" . "IEEE International Conference on Computer Vision")
    ("wavc" . "IEEE Winter Conference on Applications of Computer Vision")
    ("eccv" . "European Conference on Computer Vision")
    ("cvpr" . "IEEE Conference on Computer Vision and Pattern Recognition")
    ("iclr" . "International Conference on Learning Representations")
    ("bmvc" . "British Machine Vision Conference")
    ("aistats" . "International Conference on Artificial Intelligence and Statistics")
    ("uai" . "Conference on Uncertainty in Artificial Intelligence")
    ("ijcv" . "International Journal of Computer Vision")
    ("icml" . "International Conference on Machine Learning")
    ("pami" . "IEEE Transactions on Pattern Analysis and Machine Intelligence")
    ("tpami" . "IEEE Transactions on Pattern Analysis and Machine Intelligence")
    ("jair" . "Journal of Artificial Intelligence Research")
    ("jmlr" . "Journal of Machine Learning Research"))
  "Alist of venues and their abbreviations.")

(defconst ref-man--num-to-months
  '((1 . "Jan") (2 . "Feb") (3 . "Mar") (4 . "Apr")
    (5 . "May") (6 . "Jun") (7 . "Jul") (8 . "Aug")
    (9 . "Sep") (10 . "Oct") (11 . "Nov") (12 . "Dec")))

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
                            (a-get ref-man-venue-priorities (downcase (car (last x)))))
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
  (condition-case nil
      (if (or (string-match-p "[0-9]+" (car (last author)))
              (string-match-p "^i$\\|^ii$\\|^iii$\\|^iv$" (downcase (car (last author)))))
          (if (> (length author) 2) (butlast author) (nconc (butlast author) '("")))
        author)
    (error
     (message "Invalid author %s" author)
     '("author invalid"))))


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

;; FIXME: Check for errors in case something is nil.
;;        `ref-man-parse-properties-for-bib-key' throws error because of this
;;        subroutine as TITLE here evals to nil.
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
                                                              (butlast temp-auth) " "))
                                   ", "))))
                  authors)))
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
    (ref-man--invert-accents result-authors)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START Some experimental CSL functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ref-man-bibtex-to-csl-alist
  '(("=key=" . id)
    ("address" . publisher-place)
    ("venue" . container-title)
    ("booktitle" . container-title)
    ("journal" . container-title)
    ("chapter" . title)
    ("location" . event-place)
    ("series" . collection-title)
    ("keywords" . keyword)
    ("institution" . publisher)
    ("school" . publisher)
    ("pages" . page)
    ("organization" . publisher)
    ("url" . URL)
    ("doi" . DOI)
    ("pmid" . PMID)
    ("pmcid" . PMCID))
"Alist mapping BibTeX keys to CSL keys with different names.")

(defun ref-man-citeproc-bib-to-csl-date (year month &optional pandoc-compat)
  "Return date in CSL format.

YEAR and MONTH are the values of the corresponding BibTeX fields,
MONTH might be nil.  Derived from `citeproc-bt--to-csl-date'.
See for details.

When PANDOC-COMPAT is non-nil, date-parts aren't given and year
is given with `issued' as key."
  (let ((csl-year (string-to-number (car (s-match "[[:digit:]]+" year))))
	(csl-month (when month
		     (assoc-default (downcase month)
				    citeproc-bt--mon-to-num-alist)))
	date)
    (when csl-year
      (when csl-month (push csl-month date))
      (push csl-year date))
    (if pandoc-compat
        (string-join (mapcar #'number-to-string date) "-")
      `((date-parts . ,date)))))

(defun ref-man-bibtex-to-csl (bib &optional pandoc-compat)
  "Return a CSL form of normalized parsed BibTeX entry BIB.

Like `citeproc-bt-entry-to-csl' but optionally compatible with
`pandoc' with optional variable PANDOC-COMPAT and using
`ref-man-bibtex-to-csl-alist' for conversion."
  (let ((type (assoc-default (downcase (assoc-default "=type=" bib))
			     citeproc-bt--to-csl-types-alist))
	result year month)
    (cl-loop for (key . value) in bib do
	     (let ((key (downcase key))
		   (value (citeproc-bt--to-csl value)))
	       (-if-let (csl-key (assoc-default key ref-man-bibtex-to-csl-alist))
		   ;; Vars mapped simply to a differently named CSL var
		   (push (cons csl-key value) result)
		 (pcase key
		   ((or "author" "editor") ; Name vars
		    (push (cons (intern key) (citeproc-bt--to-csl-names value))
			  result))
		   ("=type=" (push (cons 'type type) result))
		   ("number" (push (cons (if (string= type "article-journal") 'issue
					   'number)
					 value)
				   result))
		   ;; Date vars that need further processing below
		   ("year" (setq year value))
		   ("month" (setq month value))
		   ;; Remaining keys are mapped without change
		   (_ (push (cons (intern key) value) result))))))
    (when year
      (push (cons 'issued (ref-man-citeproc-bib-to-csl-date year month pandoc-compat))
	    result))
    result))

(defun ref-man-bibtex-csl-to-yaml (maybe-bib)
  "Parse given bibtex MAYBE-BIB to csl and return yaml.
When called interactively, then read bibtex from current buffer
at point.  Also kill to kill ring when interactive."
  (interactive "p")
  (let* ((bib (cond ((numberp maybe-bib)
                     (ref-man-bibtex-to-csl (bibtex-parse-entry) t))
                    ((stringp maybe-bib)
                     (with-temp-buffer
                       (insert maybe-bib)
                       (goto-char (point-min))
                       (ref-man-bibtex-to-csl (bibtex-parse-entry) t)))))
         (csl (with-current-buffer
                  (ref-man--post-json-synchronous (ref-man-py-url "get_yaml") bib)
             (goto-char (point-min))
             (re-search-forward "\r?\n\r?\n")
             (concat (buffer-substring-no-properties (point) (point-max))  "\n"))))
    (when (numberp maybe-bib)
      (kill-new csl))
    csl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END Some experimental CSL functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
           (message "[ref-man] Waiting for Science Parse server to become ready."))
          (t
           (let*
               ((pdf-file-name (expand-file-name (buffer-file-name (current-buffer))))
                (json-string
                 (if (string-equal major-mode "pdf-view-mode")
                     (shell-command-to-string (format "curl -s -H\
            \"content-type: application/pdf\" --data-binary @%s\
            \"http://localhost:%s/v1\"" pdf-file-name ref-man-science-parse-server-port))
                   (progn (message "[ref-man] not pdf-view-mode") nil)))
                (json-object-type 'hash-table)
                (json-key-type 'string)
                (json-array-type 'list)
                (json-string (if json-string (json-read-from-string json-string) nil))
                ;; concats title and authors for each ref entry for easy lookup
                (refs-list (if json-string
                               (mapcar (lambda (x)
                                         (cons (concat (gethash "title" x) " "
                                                       (string-join (gethash "authors" x) " "))
                                               x))
                                       (gethash "references" json-string))
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
(defun ref-man--parse-json-callback (_status _url callback)
  "Callback to parse a response buffer as JSON.
STATUS is HTTP status, URL the called url, and CALLBACK is the
callback which will be called after parsing the JSON data."
  (goto-char (point-min))
  (forward-paragraph)
  (setq ref-man--json-data (json-read))
  (apply callback (list ref-man--json-data)))

(defun ref-man--post-json (url data callback)
  "Send an HTTP POST with JSON data request to URL.
QUERIES is a list of strings which is encoded as json.  The
request is sent with content-type as application/json.

CALLBACK is passed as an argument to
`ref-man--parse-json-callback' after the URL is retrieved.
`ref-man--parse-json-callback' decodes the JSON data to elisp
structures and then calls CALLBACK on it."
  (let ((url-request-extra-headers
         `(("Content-Type" . "application/json")))
        (url-request-method "POST")
        (url-request-data
         (encode-coding-string (json-encode data) 'utf-8)))
    (url-retrieve url #'ref-man--parse-json-callback
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
    (url-retrieve url #'ref-man--parse-json-callback
                  (list url callback))))

(defun ref-man--dblp-fetch-python-process-results (refs-list org-buf results)
  "Utility function to process results from the python server.

It's passed as an argument to `ref-man--post-json' but as a
partial application with list of queries REFS-LIST and target
buffer ORG-BUF fixed.  `ref-man--parse-json-callback' processes
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
    (message "Inserted %s references from DBLP, %s from SP"
             (- (length refs-list) (length na-results)) (length na-results))))

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
        (url (ref-man-py-url "dblp")))
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
         (buf (url-retrieve-synchronously query-url))
         (beg (with-current-buffer buf (set-buffer-multibyte t)
                                   (goto-char (point-min))
                                   (re-search-forward "\r?\n\r?\n")
                                   (point))))
    (pcase-let ((`(,(and result `(result . ,_)))
                 (xml-parse-region beg nil buf)))
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
  (declare (pure t) (side-effect-free t))
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
         (buf (find-buffer-visiting visiting-filename))
         open-file)
    (if (not filename)
        (message "[ref-man] filename could not be generated!")
      (setq filename (string-remove-prefix "na_" filename)) ; always remove na_ from filename
      (if buf
          (cond ((not (string-empty-p (with-current-buffer buf (buffer-string))))
                 (message "[ref-man] File is already opened and not empty. Switching...")
                 (ref-man--create-org-buffer (concat filename ".org")))
                ((string-empty-p (with-current-buffer buf (buffer-string)))
                 (message "[ref-man] Buffer is opened but is empty."))
                (t nil))
        (when (file-exists-p visiting-filename)
          (message "[ref-man] File already exists. Opening...")
          (setq open-file t)))
      (let ((org-buf (ref-man--create-org-buffer (concat filename ".org"))))
        (when open-file
          (with-current-buffer org-buf
            (insert-file-contents visiting-filename t))
          (unless (string-empty-p (with-current-buffer buf (buffer-string)))
            (message "[ref-man] Opened buffer but is empty.")))
        (ref-man--generate-org-buffer-content org-buf refs-list entry-alist visiting-filename)))))

(defun ref-man-org-find-duplicate-headings ()
  "Find duplicate headings for current heading or link under point.
Display the entries if any found in a helm buffer."
  (interactive)
  (pcase-let* ((link (util/org-link-get-target-for-internal))
               (`(,buf ,pt) (if link
                                `(,(find-file-noselect (plist-get link :file))
                                  ,(plist-get link :point))
                              `(,(current-buffer) ,(save-excursion
                                                     (org-back-to-heading t)
                                                     (beginning-of-line)
                                                     (point)))))
               (`(,heading ,cid ,buf-name) (with-current-buffer buf
                                             (save-excursion
                                               (goto-char pt)
                                               `(,(org-get-heading t t t t)
                                                 ,(org-entry-get (point) "CUSTOM_ID")
                                                 ,(buffer-name)))))
               (headings (-keep (lambda (x)
                                  (unless (and (= pt (nth 4 x)) (string= (nth 2 x) buf-name))
                                    `(,(car x) . ,(list (nth 2 x) (nth 4 x)))))
                                (ref-man-org-check-for-duplicate-pub heading cid))))
    (if headings
        (util/helm-org-headings nil headings)
      (message "No duplicate headings found"))))

(defun ref-man-org-check-for-duplicate-pub (heading cid)
  "Return list of org headings matching CUSTOM_ID or heading.

Given CID is matched with CUSTOM_ID and heading with the org HEADING.

The list structure is same as `util/org-collect-headings-cache'.
See for details."
  (util/org-filter-from-headings-cache
   nil
   (lambda (y) (or (string= cid (nth 3 y))
                   (string= (downcase heading) (downcase (car y)))))))


(defun ref-man-org-drawers-balanced-p (blocks)
  "Check if BLOCKS of drawers are balanced.
BLOCKS is an alist of beginning and end of drawers.

E.g. '((beg . 82) (end . 630) (beg . 749) (end . 1297))"
  (= (length (-filter (lambda (x) (eq (car x) 'beg)) blocks))
     (length (-filter (lambda (x) (eq (car x) 'end)) blocks))))

(defun ref-man-org-consolidate-drawer-from-data (drawer-re data)
  "Consolidate drawers from a list of drawer blocks DATA.

The data is filtered with matching DRAWER-RE and those are combined
into a single block.

The function doesn't warn about or overwrite duplicate key-value
pairs."
  (let ((drawer (-filter (lambda (x) (string-match-p drawer-re x))
                         data)))
    (cond ((= 1 (length drawer))
           (insert (car drawer))
           (insert "\n"))
          (t (let ((prop-start (car (split-string (car drawer) "\n")))
                   (prop-end (-last-item (split-string (car drawer) "\n"))))
               (insert prop-start "\n")
               (seq-do (lambda (x) (insert
                                    (string-join
                                     (cdr (-butlast (split-string x "\n"))) "\n"))
                         (insert "\n"))
                       drawer)
               (insert prop-end "\n"))))))

;; TEST: (with-current-buffer "test-org-end-of-metadata.org"
;; (ref-man-org-consolidate-drawers))
;; post the TEST
;; 1. all drawers should are after the heading and text after that
;; 2. Newlines in text aren't affected
;; 3. PROPERTIES and LOGBOOK are merged
;; 4. Rest of the drawers aren't merged
(defun ref-man-org-consolidate-drawers (&optional properties-only)
  "Place all drawers one after other with property drawer first.
When optional PROPERTIES-ONLY is non-nil, then only check for and
consolidate property drawers."
  (interactive)
  (org-back-to-heading t)
  (let* ((beg (point))
         (pblock (org-get-property-block))
         (any-block (save-restriction
                      (org-narrow-to-subtree)
                      (string-match-p org-drawer-regexp
                                      (buffer-substring-no-properties (point-min) (point-max)))))
         (end (progn (outline-next-heading) (- (point) 1)))
         (continue (if (and properties-only pblock) pblock any-block))
         blocks temp data)
    (when continue
      (save-restriction
        (save-excursion
          (narrow-to-region beg end)
          (goto-char (point-max))
          (while (re-search-backward org-drawer-regexp nil t)
            (if (equal (match-string 1) "END")
                (push `(end . ,(point-at-eol)) blocks)
              (push `(beg . ,(point-at-bol)) blocks)))
          (when blocks
            (unless (ref-man-org-drawers-balanced-p blocks)
              (user-error "Unbalanced drawers"))
            (if (ref-man-util-regions-contiguous-p blocks)
                (1+ (cdr (-last-item blocks)))
              (seq-do
               (lambda (x)
                 (if (eq (car x) 'end)
                     (push (cdr x) temp)
                   (let ((drawer (buffer-substring-no-properties (cdr x) (car temp))))
                     (when (and (string-match org-drawer-regexp drawer)
                                (match-string 1 drawer))
                       (push (buffer-substring (cdr x) (car temp)) data)
                       (delete-region (- (cdr x) 1) (car temp))))))
               (reverse blocks))
              (outline-back-to-heading t)
              (forward-line)
              (ref-man-org-consolidate-drawer-from-data org-property-start-re data)
              (ref-man-org-consolidate-drawer-from-data org-logbook-drawer-re data)
              ;; NOTE: Rest of the drawers aren't combined
              (seq-do (lambda (x) (unless (or (string-match-p org-property-drawer-re x)
                                              (string-match-p org-logbook-drawer-re x))
                                    (insert x)
                                    (insert "\n")))
                      data)
              (point))))))))

(defun ref-man-org-up-heading ()
  "Like `outline-up-heading' but go back to DOC_ROOT with \\[universal-argument]."
  (interactive)
  (pcase (and current-prefix-arg (util/org-get-tree-prop "DOC_ROOT"))
    ((and (pred integerp) pt) (goto-char pt))
    (_ (outline-up-heading 1))))

;; TEST:
;; (with-current-buffer "test-org-end-of-metadata.org"
;;   (goto-char (point-min))
;;   (ref-man-org-end-of-meta-data)
;;   )
;; Post TEST:
;; 1. The drawers should be merged.
;; 2. Point should be at the end of last drawer
(defun ref-man-org-end-of-meta-data (&optional no-consolidate no-newline)
  "Go to end of all drawers and other metadata.

With optional non-nil NO-CONSOLIDATE, do not consolidate the
drawers into a contiguous chunk.  Default is to do so.

With optional non-nil NO-NEWLINE don't add a newline at the end
of the heading content if it doesn't exist."
  (save-restriction
    (org-narrow-to-subtree)
    (let ((beg (point-min))
          (end (if (outline-next-heading)
                   (- (point) 1)
                 (point))))
      (narrow-to-region beg end)
      (let* (matches
             (pt-end-metadata (cond (no-consolidate
                                     (goto-char (point-min))
                                     (while (re-search-forward "^ +:END: *$" nil t)
                                       (push t matches))
                                     (when matches
                                       (goto-char (+ 1 (point)))
                                       (point)))
                                    (t (ref-man-org-consolidate-drawers))))
             (end (progn (goto-char (point-min))
                         (end-of-line)
                         (or (when (re-search-forward "\\*+ .+" nil t)
                               (- (point-at-bol) 1))
                             (point-max)))))
        (if pt-end-metadata
            (goto-char pt-end-metadata)
          (outline-back-to-heading t)
          (forward-line)
          (unless no-newline
            (when (org-at-heading-p)
              (open-line 1)
              (forward-line))))               ; what if we go to next heading instead?
        (when (org-at-heading-p)
          (end-of-line)
          (unless no-newline
            (unless (save-excursion
                      (looking-at "\n[ \n]+"))
              (insert "\n"))))
        (point)))))

;; TEST: empty text body should return nil
;; (with-current-buffer "test-org-end-of-metadata.org"
;;   (goto-char (point-min))
;;   (re-search-forward "Heading with logbook and no text")
;;   (ref-man-org-end-of-meta-data)
;;   (pcase-let ((`(,beg ,end ,hb) (ref-man-org-text-bounds)))
;;     (when hb (buffer-substring-no-properties beg end))))

;; TEST: Text body with newlines returns correct string
;; (let ((text "
;;    Some text entered here but no authors or other strings
;;    Some text entered here but no authors or other strings




;;    Some text entered here but no authors or other strings

;; "))
;;   (with-current-buffer "test-org-end-of-metadata.org"
;;     (goto-char (point-min))
;;     (re-search-forward
;;      "NormLime: A New Feature Importance Metric for Explaining Deep Neural Networks")
;;     (ref-man-org-end-of-meta-data)
;;     (pcase-let ((`(,beg ,end ,hb) (ref-man-org-text-bounds)))
;;       (string= text (buffer-substring-no-properties beg end)))))

(defun ref-man-org-text-bounds ()
  "Return bounds of text body if present in org subtree.

Return value is a triple of '(beg end has-body) where beg is the
point after metadata, end is the point at end of subtree and
has-body indicates if any text is present."
  (save-excursion
    (let* ((beg (or (ref-man-org-end-of-meta-data t t) (point)))
           (end (progn
                  (unless (org-at-heading-p)
                    (outline-next-heading))
                  (point)))
           (has-body (not (string-empty-p
                           (string-trim
                            (buffer-substring-no-properties beg end))))))
      (list beg end has-body))))

(defun ref-man-org-insert-prop-list-item (prop
                                          &optional buf keep-current
                                          consolidate-drawers no-newline)
  "Insert a paper property as text in entry after property drawer if it exists.

The property PROP is a key value pair and is inserted as a list
item after the current key-value props if they exist. They are
all inserted as a list like \" - key: value\"

Insert to `current-buffer' if optional BUF is nil else to BUF.

If optional KEEP-CURRENT is non-nil, keep the current text after
the heading, otherwise delete."
  (unless buf (setq buf (current-buffer)))
  (with-current-buffer buf
    (ref-man-org-end-of-meta-data (not consolidate-drawers)
                                  no-newline)
    (when (and (looking-back ":END:") (looking-at-p "[ \n]+"))
      (insert "\n"))
    (pcase-let* ((`(,key . ,val) prop)
                 (`(,beg ,end ,has-text) (ref-man-org-text-bounds))
                 (text (and has-text (buffer-substring-no-properties beg end)))
                 (splits (and text (-remove #'string-empty-p (split-string text "^ +- "))))
                 (level (org-current-level)))
      (unless (-any (lambda (x) (string-match-p (format "^%s: " key) x)) splits)
        (unless keep-current
          (when (not (= beg end))
            (delete-region beg (- end 1))))
        (goto-char beg)
        (insert (make-string (1+ level) ? ) (format "- %s: " (capitalize key))
                (replace-regexp-in-string "\n" "" val))
        (insert "\n")
        (goto-char beg)
        (org-indent-line)
        (fill-paragraph)
        (org-end-of-item)
        (when (org-at-heading-p)
          (backward-char))))))

(defun ref-man-org-insert-abstract-list-item (abs &optional buf keep-current)
  (ref-man-org-insert-prop-list-item (if (consp abs) abs (cons "abstract" abs)) buf keep-current))

;; (defun ref-man-org-insert-abstract-list-item (abs &optional buf keep-current)
;;   "Insert abstract as text in entry after property drawer if it exists.
;; ABS is the abstract string.

;; Insert to `current-buffer' if optional BUF is nil else to BUF.

;; If optional KEEP-CURRENT is non-nil, keep the current text after
;; the heading, otherwise delete."
;;   (unless buf (setq buf (current-buffer)))
;;   (with-current-buffer buf
;;     (ref-man-org-end-of-meta-data)
;;     (pcase-let* ((`(,beg ,end ,has-text) (ref-man-org-text-bounds))
;;                  (text (and has-text (buffer-substring-no-properties beg end)))
;;                  (splits (and text (-remove #'string-empty-p (split-string text "^ +- "))))
;;                  (level (org-current-level)))
;;       (unless (-any (lambda (x) (string-match-p "^abstract: " x)) splits)
;;         (unless keep-current
;;           (when (not (= beg end))
;;             (delete-region beg (- end 1))))
;;         (goto-char beg)
;;         (insert (make-string (1+ level) ? ) "- Abstract: "
;;                 (replace-regexp-in-string "\n" "" abs))
;;         (insert "\n")
;;         (goto-char beg)
;;         (org-indent-line)
;;         (fill-paragraph)
;;         (forward-line)
;;         (when (org-at-heading-p)
;;           (backward-char))))))

;; (defun ref-man-org-insert-author-list-item (authors &optional buf keep-current)
;;   "Insert AUTHORS as text in entry after property drawer if it exists.

;; Insert to `current-buffer' if optional BUF is nil else to BUF.

;; If optional KEEP-CURRENT is non-nil, keep the current text after
;; the heading, otherwise delete."
;;   (unless buf (setq buf (current-buffer)))
;;   (with-current-buffer buf
;;     (ref-man-org-end-of-meta-data)
;;     (pcase-let* ((`(,beg ,end ,has-text) (ref-man-org-text-bounds))
;;                  (text (and has-text (buffer-substring-no-properties beg end)))
;;                  (splits (and text (-remove #'string-empty-p (split-string text "^ +- "))))
;;                  (level (org-current-level)))
;;       (unless (-any (lambda (x) (string-match-p "^abstract: " x)) splits)
;;         (unless keep-current
;;           (when (not (= beg end))
;;             (delete-region beg (- end 1))))
;;         (goto-char beg)
;;         (insert (make-string (1+ level) ? ) "- Abstract: "
;;                 (replace-regexp-in-string "\n" "" abs))
;;         (insert "\n")
;;         (goto-char beg)
;;         (org-indent-line)
;;         (fill-paragraph)
;;         (forward-line)
;;         (when (org-at-heading-p)
;;           (backward-char))))))

;; (defun ref-man-org-insert-venue-year-list-item (venue-year &optional buf keep-current)
;;   "Insert venue and year as text in entry after property drawer if it exists.
;; VENUE-YEAR is a plist of venue and year.

;; Insert to `current-buffer' if optional BUF is nil else to BUF.

;; If optional KEEP-CURRENT is non-nil, keep the current text after
;; the heading, otherwise delete."
;;   (unless buf (setq buf (current-buffer)))
;;   (with-current-buffer buf
;;     (ref-man-org-end-of-meta-data)
;;     (pcase-let* ((`(,beg ,end ,has-text) (ref-man-org-text-bounds))
;;                  (text (and has-text (buffer-substring-no-properties beg end)))
;;                  (splits (and text (-remove #'string-empty-p (split-string text "^ +- "))))
;;                  (level (org-current-level)))
;;       (unless (-any (lambda (x) (string-match-p "^abstract: " x)) splits)
;;         (unless keep-current
;;           (when (not (= beg end))
;;             (delete-region beg (- end 1))))
;;         (goto-char beg)
;;         (insert (make-string (1+ level) ? ) "- Abstract: "
;;                 (replace-regexp-in-string "\n" "" abs))
;;         (insert "\n")
;;         (goto-char beg)
;;         (org-indent-line)
;;         (fill-paragraph)
;;         (forward-line)
;;         (when (org-at-heading-p)
;;           (backward-char))))))

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
\"na_\".  It happens in case DBLP or another source cannot resolve
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
           ;; FIXME: Why's this commented out? and where's na generated now?
           ;; (key (mapconcat (lambda (x) (replace-in-string (downcase x) " " ""))
           ;;                 (list "na" "_"
           ;;                       (let ((first-author (split-string (car (gethash "authors" hash)) " ")))
           ;;                         (if (= 1 (length first-author)) (car first-author)
           ;;                           (nth 1 first-author)))
           ;;                       (if (gethash "year" hash) (format "%s" (gethash "year" hash)) "_")
           ;;                       (car (split-string (gethash "title" hash) " " t))) ""))
           (key (ref-man--build-bib-key-from-plist (list :title (cdr title)
                                                         :year (cdr year)
                                                         :author (cdr author))))
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
(defun ref-man--org-bibtex-write-ref-from-ss-ref (entry &optional ignore-errors
                                                        update-current no-abstract)
  "Generate an org entry from data fetched from Semantic Scholar.

ENTRY is an alist of a bibliographic property keys and values.

Optional IGNORE-ERRORS to ignore erros in case error occurs while
parsing the org properties as bibtex.

With optional UPDATE-CURRENT, update the current org entry
properties (reflecting the bibliography data) with
semanticscholar data also.  The default is to insert a new entry
after current.

Don't insert abstract with optional non-nil NO-ABSTRACT."
  ;; NOTE: insert only when title exists
  (save-excursion
    (when (a-get entry 'title)
      ;; NOTE: insert heading only when not updating current heading
      (unless update-current
        (org-insert-heading-after-current))
      (org-edit-headline (a-get entry 'title))
      ;; FIXME: beg is same as current point
      (pcase-let ((`(,beg ,_ ,has-text) (ref-man-org-text-bounds))
                  (author-str (mapconcat (lambda (x)
                                           (a-get x 'name))
                                         (a-get entry 'authors) ", "))
                  (level (org-current-level)))
        (ref-man-org-insert-prop-list-item
         (cons "venue" (format "%s, %s" (a-get entry 'venue) (a-get entry 'year))) nil t)
        (ref-man-org-insert-prop-list-item (cons "authors" author-str) nil t)
        (when (and (not no-abstract) (a-get entry 'abstract))
          (ref-man-org-insert-abstract-list-item (cons "abstract" (a-get entry 'abstract)) nil t))
        ;; (unless has-text
        ;;   (unless (and (looking-at "\n") (looking-back "\n" 1))
        ;;     (insert "\n"))
        ;;   (insert (format "%s- Authors: %s" (make-string (1+ level) ? ) author-str))
        ;;   (org-insert-item)
        ;;   (insert (concat (a-get entry 'venue) ", " (format "%s" (a-get entry 'year)))))
        (org-insert-property-drawer)
        (cl-loop for ent in entry
                 do
                 (cond ((or (eq (car ent) 'author) (eq (car ent) 'authors))
                        (when (not (string-empty-p author-str))
                          (org-set-property "AUTHOR"
                                            (ref-man--invert-accents
                                             (ref-man--replace-non-ascii
                                              (ref-man--fix-curly
                                               (ref-man--build-bib-author author-str)))))))
                       ((eq (car ent) 'isInfluential)
                        (if (not (eq (cdr ent) 't))
                            (org-set-property "ISINFLUENTIAL" "nil")
                          (outline-back-to-heading)
                          (org-set-tags ":influential:")
                          (org-set-property "ISINFLUENTIAL" "t")))
                       ((eq (car ent) 'externalIds)
                        (seq-do
                         (lambda (x)
                           (unless (eq (car x) 'CorpusId)
                             (org-set-property (if (string= (upcase (format "%s" (car x))) "ARXIV")
                                                   "ARXIVID"
                                                 (upcase (format "%s" (car x))))
                                               (format "%s" (cdr x)))))
                         (cdr ent)))
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
        (let ((key (ref-man-parse-properties-for-bib-key)))
          (unless (or key ignore-errors)
            (debug)
            ;; FIXME: This function should filter the user read key
            ;; ref-man--build-bib-key-from-plist
            (setq key (read-from-minibuffer (format "Could not parse key:\nauthor: %s\ntitle: %s\nyear: %s"
                                                    (org-entry-get (point) "AUTHOR")
                                                    (org-entry-get (point) "TITLE")
                                                    (org-entry-get (point) "YEAR")))))
          (when key
            (org-set-property "CUSTOM_ID" key)))
        (org-set-property "BTYPE" "article"))
      ;; CHECK: Why are we doing this? When org edit headline is called earlier
      ;; anyway? COMMENTED
      ;; (when update-current
      ;;   (unless (org-at-heading-p)
      ;;     (outline-previous-heading))
      ;;   (beginning-of-line)
      ;;   (forward-whitespace 1)
      ;;   (just-one-space)
      ;;   (unless (eolp)
      ;;     (kill-line))
      ;;   (insert (cdr (assoc 'title entry))))
      )))

(defun ref-man--org-bibtex-write-ref-from-assoc (entry &optional ignore-errors
                                                       update-current no-abstract)
  "Write an org entry a bib ENTRY which is an alist of strings.

The arguments mean the same as `ref-man--org-bibtex-write-ref-from-ss-ref'."
  (let* ((key (car entry))
         (entry (nth 1 entry)))
    (org-insert-heading-after-current)
    (insert (cdr (assoc "title" entry)))
    (insert "\n")
    (org-indent-line)
    (when (and (not no-abstract) (assoc "abstract" entry))
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

(defun ref-man--org-bibtex-write-ref-from-plist (entry &optional ignore-errors
                                                       update-current no-abstract)
  "Write an org entry a bib ENTRY which is a plist.

The arguments mean the same as `ref-man--org-bibtex-write-ref-from-ss-ref'."
  (org-insert-heading-after-current)
  (insert (cdr (assoc :title entry)))
  (insert "\n")
  (org-indent-line)
  (when (and (not no-abstract) (assoc :abstract entry))
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
  (let ((key (ref-man-parse-properties-for-bib-key)))
    (unless key
      ;; FIXME: This function should filter the user read key
      ;; ref-man--build-bib-key-from-plist
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

;; FIXME: How's this different from earlier org generation functions?
(defun ref-man--org-bibtex-write-heading-from-bibtex (entry &optional ignore-errors
                                                            update-current no-abstract)
  "Write an org entry from an alist parsed from a bibtex ENTRY.

The arguments are same as `ref-man--org-bibtex-write-ref-from-ss-ref'."
  (org-insert-heading)
  (insert (ref-man--fix-curly (cdr (assoc "title" entry))))
  (insert "\n")
  ;; from where do I get the abstract?
  ;; assuming abstract is in the bib entry
  (when (and (not no-abstract) (assoc "abstract" entry))
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

(defun ref-man-bibtex-parse-buffer (buf)
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
             (entries (ref-man-bibtex-parse-buffer (find-file-noselect filename 'nowarn))))
        ;; (setq org-bibtex-entries nil)
        ;; (org-bibtex-read-buffer (find-file-noselect filename 'nowarn))
        (with-current-buffer org-buf
          (ref-man--insert-refs-from-seq
           entries nil 'bibtex)))
    (message "[ref-man] File %s does not exist" filename)))

(defun ref-man-org-bibtex-kill-headline-as-bibtex ()
  "Parse the headline at point, convert to a bib entry and append to `kill-ring'.
Only for interactive use.  Same as
`ref-man-org-bibtex-read-from-headline' with CLEAN argument,
except it returns nothing and kills the entry as bibtex.

With a single prefix \\[universal-argument], misc entries are also parsed.

See also, `ref-man-org-bibtex-insert-headline-as-bib-to-file'."
  (interactive)
  (ref-man-org-bibtex-read-from-headline t nil current-prefix-arg t))

(defun ref-man-org-bibtex-kill-headline-as-yaml ()
  "Parse the headline at point, convert to yaml and append to `kill-ring'.
Only for interactive use.  Same as
`ref-man-org-bibtex-read-from-headline' except it returns nothing
and kills the entry as yaml to be used with pandoc.

See also, `ref-man-org-bibtex-kill-headline-as-bibtex'."
  (interactive)
  (let* ((bib (ref-man-org-bibtex-read-from-headline))
         (yaml (replace-regexp-in-string
                "^---\\|^nocite.*\\| +url: .+" ""
                (shell-command-to-string (format "%s -s -f biblatex -t markdown <<<'%s'"
                                                 ref-man-pandoc-executable
                                                 bib)))))
    (kill-new yaml)))

(defun ref-man-org-publish-type (props-alist &optional allow-misc)
  "Get the correct published type for an org entry properties PROPS-ALIST.
Optional ALLOW-MISC allows misc and unpublished."
  (let ((venue (a-get props-alist "VENUE"))
        (booktitle (a-get props-alist "BOOKTITLE"))
        (journal (a-get props-alist "JOURNAL"))
        (type (or (a-get props-alist "TYPE") (a-get props-alist "BTYPE"))))
    (cond ((and type (string= (downcase type) "book"))
           "book")
          ((and journal (not (string-match-p "arxiv\\|corr" journal)))
           (setq props-alist (a-dissoc props-alist "VENUE" "BOOKTITLE"))
           "article")
          ((and booktitle (not (string-match-p "arxiv\\|corr" booktitle)))
           (setq props-alist (a-dissoc props-alist "JOURNAL" "VENUE"))
           "inproceedings")
          ((and venue (not (string-match-p "arxiv\\|corr" venue)))
           (setq props-alist (a-dissoc props-alist "JOURNAL" "BOOKTITLE"))
           "inproceedings")
          (t (if allow-misc "misc" "unpublished")))))

(defun ref-man-key-from-url (url)
  "Generate a bibtex key from a URL."
  (concat (time-stamp-string "%Y")
          (replace-regexp-in-string
           "%[0-9][0-9]" "_"
           (f-base
            (-last-item
             (split-string
              (car (url-path-and-query
                    (url-generic-parse-url url)))
              "/"))))))

(defvar ref-man-bibtex-clean-pipe
  '(ref-man-bibtex-remove-arxiv ref-man-bibtex-change-venue-to-booktitle)
  "Pipe to run for cleaning a bibtex alist.
Like a hook the functions in the pipe are called in order but
they should each return the transformed variable which they take
as input.")


(defun ref-man-bibtex-change-venue-to-booktitle (bib-alist)
  "Change venue to booktitle in a BIB-ALIST.
Primarily for compatibility with pandoc citeproc which seems to
ignore the venue key."
  (when (string= (a-get bib-alist "type") "@inproceedings")
    (let ((venue (a-get bib-alist "venue")))
      (when venue
        (setq bib-alist (a-dissoc (a-assoc bib-alist "booktitle" venue) "venue")))))
  bib-alist)

(defun ref-man-bibtex-bib-is-only-arxiv-p (bib-alist)
  "Check if a bibliographic entry BIB-ALIST has only appeared in arxiv."
  (let ((case-fold-search t)
        (doi (a-get bib-alist "doi"))
        (journal (a-get bib-alist "journal"))
        (venue (a-get bib-alist "venue")))
    (or (and (not doi) journal (not venue)
             (string-match-p journal "^arxiv$\\|^corr$"))
        (and (not doi) (not journal) venue
             (string-match-p venue "^arxiv$\\|^corr$"))
        (and (not doi) journal venue
             (and (string-match-p journal "^arxiv$\\|^corr$")
                  (string-match-p venue "^arxiv$\\|^corr$"))))))

(defun ref-man-bibtex-remove-arxiv (bib-alist)
  "Remove ArXiv from a BIB-ALIST unless it's the only venue."
  (let ((only-arxiv (ref-man-bibtex-bib-is-only-arxiv-p bib-alist)))
    (unless only-arxiv
      (when (and (a-get bib-alist "journal")
                 (or (string= (downcase (a-get bib-alist "journal")) "arxiv")
                     (string= (downcase (a-get bib-alist "journal")) "corr")))
        (setq bib-alist
              (a-dissoc bib-alist "journal")))
      (when (and (a-get bib-alist "venue")
                 (string= (downcase (a-get bib-alist "venue")) "arxiv"))
        (setq bib-alist
              (a-dissoc bib-alist "venue")))))
  (when (and (a-get bib-alist "volume")
             (string-match-p "^abs/.+" (a-get bib-alist "volume")))
    (setq bib-alist (a-dissoc bib-alist "volume")))
  bib-alist)

(defun ref-man-org-parse-entry-as-bib (&optional gdrive allow-misc clean)
  "Parse the headline at point and return alist.
The entry is appended to `ref-man-bibtex-save-ring'.

When optional GDRIVE is non-nil include a (possible) gdrive link
from `ref-man-public-links-cache'.

When optional ALLOW-MISC is non-nil, if a url is found in the
property drawer, then the entry is exported as a \"@misc\" with a
\"howpublished\" and a \"url\" field.

When optional CLEAN is non-nil, some cleaning, like removing
redundant entries and arxiv metadata is done."
  (if (eq major-mode 'org-mode)
      (let* ((props (org-entry-properties))
             (bib-alist (list
                       (cons "type" (concat "@" (ref-man-org-publish-type props allow-misc)))
                       (cons "key"  (or (cdr (assoc "CUSTOM_ID" props))
                                        (ref-man-parse-properties-for-bib-key)))
                       (cons "title"  (cdr (assoc "TITLE" props)))
                       (cons "author"  (cdr (assoc "AUTHOR" props)))
                       (cons "journal"  (cdr (assoc "JOURNAL" props)))
                       (cons "publisher"  (cdr (assoc "PUBLISHER" props)))
                       (cons "venue"  (cdr (assoc "VENUE" props)))
                       (cons "booktitle"  (cdr (assoc "BOOKTITLE" props)))
                       (cons "volume"  (cdr (assoc "VOLUME" props)))
                       (cons "number"  (cdr (assoc "NUMBER" props)))
                       (cons "month"  (cdr (assoc "MONTH" props)))
                       (cons "year"  (cdr (assoc "YEAR" props)))
                       (cons "pages"  (cdr (assoc "PAGES" props)))
                       (cons "doi"  (cdr (assoc "DOI" props)))
                       (cons "url" (or (cdr (assoc "URL" props))
                                       (cdr (assoc "PDF_URL" props))))
                       (cons "gdrive" (and (cdr (assoc "PDF_FILE" props))
                                           (gethash (replace-regexp-in-string
                                                     ref-man-org-file-link-re "\\2"
                                                     (cdr (assoc "PDF_FILE" props)))
                                                    ref-man-public-links-cache)))
                       (cons "publisher"  (cdr (assoc "PUBLISHER" props)))
                       (cons "organization"  (cdr (assoc "ORGANIZATION" props)))))
             (bib-alist (if (a-get bib-alist "key") bib-alist
                        (user-error "Could not generate key for \"%s\""
                                    (org-get-heading t t t t))))
             (bib-alist (if (and gdrive (assoc "gdrive" bib-alist))
                          bib-alist (delq (assoc "gdrive" bib-alist) bib-alist)))
             (bib-alist (if (and allow-misc (a-get bib-alist "url")
                               (equal (a-get bib-alist "type") "@misc"))
                          (let ((temp (a-assoc bib-alist
                                               "howpublished" (format "\\url{%s}" (a-get bib-alist "url"))
                                               "url" nil)))
                            (unless (a-get temp "title")
                              (setq temp (a-assoc temp "title" (substring-no-properties (org-get-heading t t t t)))))
                            (unless (a-get temp "year")
                              (setq temp (a-assoc temp "year" (time-stamp-string "%Y"))))
                            (unless (a-get temp "key")
                              (setq temp (a-assoc temp "key" (ref-man-key-from-url (a-get bib-alist "url")))))
                            temp)
                        bib-alist))
             (bib-alist (mapcar (lambda (x) `(,(car x) . ,(if (cdr x)
                                                              (ref-man--replace-non-ascii (cdr x))
                                                            (cdr x))))
                                bib-alist)))
        ;; (when clean
        ;;   (when (and (a-get bib-alist "journal")
        ;;              (string= (downcase (a-get bib-alist "journal")) "arxiv"))
        ;;     (setq bib-alist
        ;;           (a-dissoc bib-alist "journal")))
        ;;   (when (and (a-get bib-alist "volume")
        ;;                (string-match-p "^abs/.+" (a-get bib-alist "volume")))
        ;;       (setq bib-alist (a-dissoc bib-alist "volume")))
        ;;   (when (string= (a-get bib-alist "type") "@inproceedings")
        ;;     (when (a-get bib-alist "venue")
        ;;       (setq bib-alist (a-assoc bib-alist "booktitle" (a-get bib-alist "venue")))
        ;;       (setq bib-alist (a-dissoc bib-alist "venue")))))
        (when clean
          (setq bib-alist
                (ref-man-util-pipe-through ref-man-bibtex-clean-pipe bib-alist)))
        bib-alist)
    (message "[ref-man] Not org mode") nil))

(defun ref-man-org-bibtex-read-from-headline (&optional kill gdrive allow-misc clean)
  "Parse the headline at point and convert to bibtex string.

The current org entry is parsed with `ref-man-org-parse-entry-as-bib'.

When optional KILL is non-nil, the entry is also added to
`kill-ring' with `kill-new'.

For the rest of the optional arguments see
`ref-man-org-parse-entry-as-bib'."
  (let* ((bib-str (ref-man-org-parse-entry-as-bib gdrive allow-misc clean))
         (header (concat (cdr (assoc "type" bib-str)) "{" (cdr (assoc "key" bib-str)) ",\n"))
         (bib-str (delq (assoc "type" bib-str) bib-str))
         (bib-str (delq (assoc "key" bib-str) bib-str))
         (bib-str (concat header
                          (mapconcat (lambda (x)
                                       (if (cdr x) (concat "  " (car x) "={"
                                                           ;; (if (equal (car x) "title")
                                                           ;;     (concat "{" (util/title-case (cdr x)) "}")
                                                           ;;   (cdr x))
                                                           (cdr x)
                                                           "},\n")))
                                     bib-str "")
                          "}\n")))
    (when kill
      (kill-new bib-str))
    (push bib-str ref-man-bibtex-save-ring)
    bib-str))

;; TODO: prefix arg should insert to temp-file in interactive mode
(defun ref-man-org-bibtex-insert-headline-as-bib-to-file (&optional file)
  "Export current headline as bibtex.

Where to export depends on various facts.  When optional FILE is
non-nil, get the current buffer visiting that file use that, else
find the file and open it.  If FILE is not given then insert to
`ref-man-temp-bib-file-path'.

In all cases the bibtex entry is inserted at the top of the
buffer.

See also, `ref-man-org-bibtex-kill-headline-as-bibtex'."
  (interactive)
  (let* ((result (ref-man-org-bibtex-read-from-headline)))
    (let* ((bib-file-path (if file file ref-man-temp-bib-file-path))
           (bib-file-name (file-name-nondirectory bib-file-path))
           (buf (if (get-buffer bib-file-name) (get-buffer bib-file-name)
                  (find-file-noselect bib-file-path))))
      (with-current-buffer buf
        (goto-char (point-min)) (insert result)
        (message "Inserted entry to %s" bib-file-name)))))

(defun ref-man-org-bibtex-yank-bib-to-property (&optional new)
  "Yank the `current-kill' to org entry properties.
The current kill is parsed as bibtex entry first.  If some fields
are present in both bibtex and property, they are overwritten.
Fields not present in bib entry are not deleted.

If the headline is empty then the headline is also inserted from
\"title\" in the bib entry.

If optional NEW or `current-prefix-arg' non-nil then create a new
heading first."
  (interactive)
  (let ((bib-assoc (with-temp-buffer
                     (yank)
                     (goto-char (point-min))
                     (bibtex-parse-entry)))
        heading)
    (when (or new current-prefix-arg)
      (org-insert-heading-respect-content))
    (ref-man-org-bibtex-convert-bib-to-property
     bib-assoc (current-buffer) (point) t)
    (setq heading (save-excursion
                    (outline-back-to-heading)
                    (org-get-heading t t t t)))
    (cond ((string-empty-p heading)
           (org-edit-headline (org-entry-get (point) "TITLE")))
          ((and (not (string= (org-get-heading t t t t) (org-entry-get (point) "TITLE")))
                (y-or-n-p "Heading and Title differ.  Update? "))
           (org-edit-headline (org-entry-get (point) "TITLE"))))))

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

(defun ref-man-org-bibtex-transform-author-bib-to-org (str)
  "Change a bibtex author string STR to regular text."
  (->> str
       (ref-man--fix-curly)
       (ref-man--trim-and-unquote)
       (ref-man--invert-accents)))

(defun ref-man-org-bibtex-transform-author-org-to-bib (str)
  "Change an author string STR suitable for bibtex."
  (ref-man--replace-non-ascii str))

;; TODO: Ignores should be customizable
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
              (`("author" . ,_) (org-set-property "AUTHOR"
                                                  (ref-man-org-bibtex-transform-author-bib-to-org
                                                   (cdr ent))))
              (`(,_ . ,_) (org-set-property (upcase (car ent))
                                            (ref-man-org-bibtex-transform-author-bib-to-org
                                             (cdr ent))))))
       ;; Put key generated by own rules
      (org-set-property "CUSTOM_ID" (ref-man-parse-properties-for-bib-key)))))

;; TODO: "No property drawer" should not come when property drawer is present
;; CHECK: Why only called from `ref-man--parse-bibtex'?
(defun ref-man--sanitize-org-entry (&optional org-buf org-point)
  "Sanitize an org entry.

If optional ORG-BUF is given then use that buffer else `current-buffer'.

Go to optional point ORG-POINT or
`ref-man--org-gscholar-launch-buffer' if it's non-nil.  Otherwise
sanitize the entry at current point."
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
      ('error (message "[ref-man] Caught exception: [%s]" ex)))
    (message "[ref-man] %s" retval)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END Org generation and insertion stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;; START Biblio stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(defun biblio-crossref-backend (_arg)
  "Not implemented ARG.")

(defun ref-man-org-search-heading-on-crossref-with-biblio ()
  "Search for the heading at point on crossref with `biblio'."
  (interactive)
  (setq ref-man--org-gscholar-launch-point (point))
  (setq ref-man--org-gscholar-launch-buffer (current-buffer))
  (save-excursion
    (let* ((query (org-get-heading t t t t))
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
(defun ref-man-pandoc-bib-string (&optional bib-file)
  "Generate the bibliography part of pandoc yaml metadata.
The files included are those in `ref-man-bib-files'.  With
optional BIB-FILE, include that also."
  (let ((bib-files (if bib-file
                       (cons bib-file ref-man-bib-files)
                     ref-man-bib-files)))
    (concat "bibliography:\n"
            (mapconcat (lambda (x) (concat " - " x)) bib-files "\n"))))

(defalias 'ref-man-org-delete-file-under-point 'util/org-delete-file-under-point)
(defalias 'ref-man-org-move-file-under-point 'util/org-move-file-under-point)

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
           (substring-no-properties (org-get-heading t t t t)))
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
      (cond ((and (buffer-file-name org-buf) ; It's nil if temp file
                  (f-equal? (buffer-file-name org-buf)
                            ref-man-org-links-file-path))
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
      (message "[ref-man] Imported entry \"%s\" into buffer <%s>" title (buffer-name org-buf))
      ;; restore ref-man--org-gscholar-launch-point but return point
      (prog1 (list :buffer (current-buffer)
                   :heading (substring-no-properties (org-get-heading))
                   :point (point))
        (when (and ref-man--org-gscholar-launch-buffer ref-man--org-gscholar-launch-point)
          (goto-char ref-man--org-gscholar-launch-point))))))

(defun ref-man--insert-org-pdf-url-property (url)
  "Insert a PDF_URL property from URL to an org drawer.
Confirm in case the property PDF_URL exists."
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

STATUS is response status.
URL is the url.

Rest of the ARGS are a plist."
  (unless (plist-get status :error)
    (let ((file (ref-man-files-filename-from-url url))
          (buf (and args (plist-get args :buffer)))
          (pt (and args (plist-get args :point)))
          (buf-type (ref-man--check-response-buffer (current-buffer)))
          (heading (and args (plist-get args :heading))))
      (cond ((eq buf-type 'html)
             (message "[ref-man] Got html buffer for url %s. Not saving file" url))
            ((eq buf-type 'pdf)
             (write-region (point) (point-max) file)
             (message "[ref-man] Saved %s" file))
            (t (message "[ref-man] Got uknown format for file")))
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
(make-obsolete 'ref-man--eww-pdf-download-callback-store 'ref-man--eww-pdf-download-callback-store-new
               "ref-man 0.3.2")

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

;; FIXME: This should use `ref-man--fetch-from-pdf-url-new'
;; NOTE: Used only by `ref-man--eww-pdf-download-callback'
(defun ref-man--handle-json-response (json-data &optional storep)
  "Handle JSON response from `url-retrieve'.
JSON-DATA is parsed and sent by `ref-man--eww-pdf-download-callback'.

If it's a 'redirect then ask user if we should follow the
redirect.  Optional argument STOREP is for batch mode."
  (when (equal '(redirect content) (-map #'car json-data))
    (let ((pdf-url (ref-man-url-get-pdf-url-according-to-source (a-get json-data 'redirect))))
      (cond ((and pdf-url (y-or-n-p (format "Got redirect to %s.  Fetch? " pdf-url)) (not storep))
             (ref-man--fetch-from-pdf-url pdf-url))
            ((and pdf-url storep)
             (ref-man--fetch-from-pdf-url pdf-url storep))
            ((not pdf-url)
             (message "[ref-man] handle-json-response, not a PDF URL in redirect.")
             (unless storep (debug)))
            (t (message "[ref-man] handle-json-response, not sure what to do"))))))

;; NOTE: Used by `ref-man--fetch-from-pdf-url' which is obsolete and
;;       `ref-man-eww-download-pdf'.  `ref-man-eww-download-pdf' is used by a lot
;;       of functions in `ref-man-web' so has to be fixed carefully.
(defun ref-man--eww-pdf-download-callback (status url &optional view overwrite)
  "Callback for `url-retrieve' pdf download.
STATUS is response status.  URL is the url that was fetched.

With optional argument VIEW non-nil, view the pdf file also.
OVERWRITE specifies to overwrite the target pdf file without
confirmation if it exists.

If `ref-man--org-gscholar-launch-buffer' is non-nil then PDF-FILE
property in the property drawer for the org entry at point is
inserted or updated.  See `ref-man--insert-org-pdf-file-property'."
  (if (plist-get status :error)
      (message "[ref-man] Error occured while download %s" url)
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
  "Get level of current heading.

Goto optional POINT first if given."
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
raw-link is returned.  When it equals 'path then return only the
path value.  Helpful for `file' links sometimes.

is non-nil only the link path is
returned."
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
         (bib-url (ref-man-py-url "arxiv" `(("id" . ,arxiv-id)))))
    (message "[ref-man] Fetching for arxiv-id %s" arxiv-id)
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
         (message "[ref-man] No way to get bib buffer for %s" url)
         nil)))

(defun ref-man-org-get-ss-id ()
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
        (t
         (let ((url (-some (lambda (x) (org-entry-get (point) x)) ref-man-url-org-prop-types)))
           (when url
             (cond ((string-match-p "[http\\|https]://.*?doi" url)
                    (cons 'doi (string-join (last (split-string url "/") 2) "/")))
                   ((string-match-p "https?://arxiv.org" url)
                    (cons 'arxiv (ref-man-url-to-arxiv-id url)))
                   ((string-match-p "aclweb.org\\|aclanthology.info\\|aclanthology.org" url)
                    (cons 'acl
                          (replace-regexp-in-string
                           "\\.pdf$" ""
                           (car (last (split-string (string-remove-suffix "/" url) "/"))))))
                   ((string-match-p "semanticscholar.org" url)
                    (cons 'ss (-last-item (split-string (string-remove-suffix "/" url) "/"))))
                   (t nil)))))))

(defun ref-man--insert-refs-from-seq (data name seqtype &optional ignore-errors no-abstract)
  "Insert references from a given sequence at cursor.

DATA is json-data from semantic scholar.
NAME is the org heading that will be generated.

SEQTYPE specifies the function to format the org entry
determined.  It can be one of 'ss 'assoc 'plist or 'bibtex.

After the heading is generated, each element of the data is
inserted as a reference.

With optional IGNORE-ERRORS non-nil, ignore any errors that may
happen while inserting references in the buffer.

Non-nil NO-ABSTRACT means to not insert the abstract."
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
    (if (not write-func)
        (message "[ref-man] Illegal seqtype %s" seqtype)
      (when name
        (insert name))
      (org-insert-heading-respect-content)
      (org-demote)
      (seq-do (lambda (ref)
                (funcall write-func ref ignore-errors nil no-abstract))
              data)
      (outline-up-heading 1)
      (forward-line)
      (kill-line)
      (delete-blank-lines)
      (outline-previous-heading))))

(defun ref-man-org-insert-ss-data-subr (ss-data &optional buf where ignore-errors only
                                            no-abstract current)
  "Insert Semantic Scholar Data SS-DATA into an org buffer.

SS-DATA is an alist of a paper details with references and
citations.  The heading is inserted first for the paper, then
abstract and then references and citations.  Rest of metadata is
not inserted.  The function accepts following optional arguments:

BUF is the target buffer, defaults to `current-buffer'.

WHERE specifies the point at which to insert the data.  Defaults to `point'

IGNORE-ERRORS if non-nil, indicates to ignore any errors that may happen
while inserting references in the buffer.

ONLY if given can be one of 'refs or 'cites and means to insert
either of References or Citations for an SS entry.

NO-ABSTRACT if non-nil, do not insert abstract at the top heading.

CURRENT specifies to update current heading instead of creating a
new heading.

The function assumes that it is at an org heading and inserts abstract
following the property drawer for the heading and references and
citations after that."
  (unless buf (setq buf (current-buffer)))
  (unless where (setq where (with-current-buffer buf (point))))
  ;; abstract should just be inserted as text
  (unless no-abstract
    (let ((abs (a-get ss-data 'abstract)))
      (when abs (ref-man-org-insert-abstract-list-item abs buf))))
  ;; Delete empty lines from here till end
  ;; They should all be empty lines
  (when (= (point) (point-at-bol))
    (backward-char))
  (delete-region (point) (point-max))
  (unless current
    (org-insert-heading-respect-content))
  (org-demote)                          ; demote only once
  (pcase only
    ('refs (ref-man--insert-refs-from-seq
            (a-get ss-data 'references) "references" 'ss ignore-errors t)
           (org-insert-heading-respect-content)
           (ref-man--insert-refs-from-seq nil "citations" 'ss ignore-errors t))
    ('cites (ref-man--insert-refs-from-seq nil "citations" 'ss ignore-errors t)
            (org-insert-heading-respect-content)
            (ref-man--insert-refs-from-seq
             (a-get ss-data 'citations) "citations" 'ss ignore-errors t))
    (_ (ref-man--insert-refs-from-seq
        (a-get ss-data 'references) "references" 'ss ignore-errors t)
       (org-insert-heading-respect-content)
       (ref-man--insert-refs-from-seq
        (a-get ss-data 'citations) "citations" 'ss ignore-errors t)))
  (outline-up-heading 1)
  (org-hide-block-all))

(defun ref-man-org-get-bib-from-org-link-subr (get-key &optional allow-misc clean)
  "Subroutine to get a bibtex string from an org heading.

The return value is a list with PATH.

If GET-KEY is non-nil then return a list of the bibtex key and
the bibtex entry string.

Option CLEAN means to clean the bibtex entry by running it
through `ref-man-bibtex-clean-pipe'.

The bibtex string is extracted with `ref-man-org-bibtex-read-from-headline'.

With optional non-nil ALLOW-MISC, headings which have been
published as urls are also exported as \"@misc\"."
  (let* ((url (or (org-entry-get (point) "URL")
                  (org-entry-get (point) "PDF_URL")))
         (url-key (when url (ref-man-key-from-url url)))
         (cid (org-entry-get (point) "CUSTOM_ID"))
         (key (if allow-misc (or cid url-key) cid))
         (bib (when key
                (ref-man-org-bibtex-read-from-headline nil t allow-misc clean))))
    (when bib
      (if get-key (list key bib) bib))))

(defun ref-man-org-get-bib-from-org-link (&optional get-key allow-misc clean)
  "Get a possible bibtex from an org link in text.

The link must point to either an org heading or an org
CUSTOM_ID.

With optional non-nil GET-KEY return a list of '(path key bib).  Default is
to return bib only.

CLEAN is passed on to `ref-man-org-get-bib-from-org-link-subr'.

With optional non-nil ALLOW-MISC @misc bibs are also parsed."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((link (util/org-link-get-target-for-internal))
             (search-path (when link (plist-get link :path)))
             (path (when link (plist-get link :file)))
             (buf (when path (find-file-noselect path)))
             (pt (when path (plist-get link :point))))
        (when (and buf pt)
          (with-current-buffer buf
            (save-excursion
              (goto-char pt)
              (if get-key
                  (cons search-path (ref-man-org-get-bib-from-org-link-subr get-key allow-misc clean))
                (ref-man-org-get-bib-from-org-link-subr get-key allow-misc clean)))))))))

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
(defun ref-man-fix-drawers-deleted-files ()
  "Remove files missing from `ref-man-documents-dir' in property drawers."
  (interactive)
  (if (eq major-mode 'org-mode)
      (save-excursion
        (let ((case-fold-search nil)
              files)
          (goto-char (point-min))
          (while (re-search-forward "^ +:PDF_FILE:\\(.+?\\)\n" nil t)
            (let ((file (save-match-data
                          (let* ((match (substring-no-properties (match-string 1)))
                                 (match (string-trim match)))
                            (replace-regexp-in-string
                             org-link-bracket-re "\\1"
                             match)))))
              (unless (f-exists-p file)
                (push file files)
                (replace-match ""))))
          (if files
              (message "[ref-man] Removed pdf file properties for %s files: %s" (length files) files)
            (message "[ref-man] No links to deleted files"))))
    (message "[ref-man] Not in org-mode")))

(defun ref-man-org-get-bounds-before-references ()
  "Get the bounds from point before org heading \"References\" in current subtree."
  (save-restriction
    (let ((pt-max (point-max))
          (pt-min (point-min)))
      (org-end-of-meta-data)
      (narrow-to-region (point) (point-max))
      (goto-char (point-max))
      (while (re-search-backward org-complex-heading-regexp nil t)
        (when (equal (downcase
                      (substring-no-properties (org-get-heading t t t t)))
                     "references")
          (setq pt-max (- (point) 1))))
      (list pt-min pt-max))))

(defun ref-man-org-ask-to-set-property (entry)
  "Subroutine for checking and entering blog metadata from properties drawer.
ENTRY is the `org' entry at point."
  (unless (org-entry-get (point) entry)
    (when (y-or-n-p (format "The property %s is empty.  Enter? " entry))
      (org-entry-put (point) entry
                     (read-string (format "Enter the value for %s: " entry)))))
  (org-entry-get (point) entry))


;; TODO: Send signal to flask server to update cache, though it'll be done next
;;       time it starts
(defun ref-man-org-purge-entry ()
  "Remove the given org entry and associated files from the disk."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (when (outline-next-heading)
      (user-error "Cannot purge if heading has subheadings")))
  ;; TODO: promote all subheadings one level
  ;;       This taken from [[https://stackoverflow.com/questions/24130487/emacs-org-promote-all-headings-of-a-subtree-during-export][SO]]
  ;;       (defun org-promote-to-top-level ()
  ;;         "Promote a single subtree to top-level."
  ;;         (let ((cur-level (org-current-level)))
  ;;           (loop repeat (/ (- cur-level 1) (org-level-increment))
  ;;                 do (org-promote-subtree))))
  (when (org-entry-get (point) "PDF_FILE")
    (delete-file (replace-regexp-in-string "\\[\\[\\(.+?\\)\\]\\(\\[*.*\\]*\\)\\]" "\\1"
                                           (org-entry-get (point) "PDF_FILE"))))
  (when (org-entry-get (point) "PAPERID")
    (delete-file (path-join ref-man-py-data-dir (org-entry-get (point) "PAPERID"))))
  (org-copy-subtree 1 t))

(defun ref-man-org-update-from-from-crossref ()
  "Update the current org heading from crossref."
  (interactive)
  (let* ((doi (org-entry-get (point) "DOI"))
         (journal (org-entry-get (point) "JOURNAL"))
         (url "https://api.crossref.org/works/%s/transform/application/x-bibtex")
         (bib (when doi
                (with-current-buffer (url-retrieve-synchronously (format url doi))
                  (goto-char (point-min))
                  (search-forward "@")
                  (backward-char)
                  (bibtex-parse-entry)))))
    ;; Remove erroneous arxiv value as journal
    (when (and journal (string= (downcase journal) "arxiv"))
      (org-entry-delete (point) "JOURNAL")
      (org-entry-delete (point) "VOLUME"))
    (when bib
      (cl-loop for ent in bib
            do
            (pcase ent
              (`("abstract" . ,_))
              (`("=type=" . ,_) (org-set-property "BTYPE" (ref-man--fix-curly (cdr ent))))
              (`("=key=" . ,_) nil)     ; Ignore =key=
              (`("timestamp" . ,_) nil)     ; Ignore timestamp
              (`("url" . ,_) nil)     ; Ignore url
              (`("author" . ,_) (org-set-property "AUTHOR" (ref-man--invert-accents
                                                            (ref-man--replace-non-ascii
                                                             (ref-man--trim-and-unquote
                                                              (ref-man--fix-curly (cdr ent)))))))
              (`(,_ . ,_) (org-set-property (upcase (car ent)) (ref-man--invert-accents
                                                                (ref-man--replace-non-ascii
                                                                 (ref-man--trim-and-unquote
                                                                  (ref-man--fix-curly (cdr ent))))))))))))

;; TODO: Replace all property setters in this section with
;;       ref-man--insert-org-pdf-file-property
;; TODO: Fix redundancies in bib fetching and pdf fetching
(defun ref-man-try-fetch-bib-insert-as-org-heading (&optional fetch-pdf)
  "Fetch the bib from URL in property drawer if present and update.
With optional non-nil FETCH-PDF also try to fetch a PDF file."
  ;; NOTE: This is how it should be but for now only arxiv
  ;; (let* ((url (cdr (assoc "URL" (org-entry-properties))))
  ;;        (bib-url (ref-man--try-get-bib-according-to-source url)))
  ;;   (if bib-url
  ;;       (ref-man-eww--browse-url bib-url nil (current-buffer))
  ;;     (message "[ref-man] No bibtex URL in %s" url)))
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
          (message "[ref-man] Could not parse bibtex from %s" url))
        (when (and fetch-pdf (ref-man-url-downloadable-pdf-url-p url))
          (message "[ref-man] Fetching pdf for url %s" url)
          ;; CHECK: Is it possible to not fetch bib and pdf separately but simultaneously instead?
          (ref-man--fetch-from-pdf-url-new url)))
    (message "[ref-man] Not in org-mode")))

;; FIXME: This could be `let'
(defvar ref-man-convert-links-in-subtree-to-headings-fetch--pdfs)
(defun ref-man-convert-links-in-subtree-to-headings ()
  "Convert all the links in the subtree to headings.

If the link is from a recognized parseable host.  As of now, only
ArXiv"
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

(defun ref-man-bibtex-kill-as-org ()
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

(defun ref-man-org-add-url-property (url)
  "Add a new URL property to an org heading.

If an existing url exists in the properties and is different from
the current url, then it is handled as following:

- If the existing url property is one of type '(arxiv ss doi)
  then it's renamed as ARXIV_URL etc. instead.
- If the new url is one of type '(arxiv ss doi) then that is
  named as ARXIV_URL etc. and the existing URL property remains
  URL.
- If none of the above then it's renamed to ALT_URL."
  (let* ((existing (org-entry-get (point) "URL"))
         (site-a (when existing (or (ref-man-url-meta-url existing) 'alt)))
         (site-b (when existing (or (ref-man-url-meta-url url) 'alt)))
         (type (cdr (-first (lambda (x) (string-match-p (car x) url))
                            ref-man-url-types))))
    (if (and existing (not (string= url existing)))
        (cond
         ((and (eq site-a 'alt) (eq site-b 'alt))
          (org-entry-put (point) "ALT_URL" existing))
         ((and (memq site-a '(arxiv ss doi)) (eq site-b 'alt))
          (org-entry-put (point) (format "%s_URL" (upcase (symbol-name site-a))) existing)
          (org-entry-put (point) "URL" url))
         ((and (eq site-a 'alt) (memq site-b '(arxiv ss doi)))
          (org-entry-put (point) (format "%s_URL" (upcase (symbol-name site-b))) url))
         ((and (memq site-a '(arxiv ss doi)) (memq site-b '(arxiv ss doi)))
          (org-entry-put (point) (format "%s_URL" (upcase (symbol-name site-a))) existing)
          (org-entry-put (point) (format "%s_URL" (upcase (symbol-name site-b))) url))
         (t (org-entry-put (point) "URL" url)))
      (if type
          (org-entry-put (point) (format "%s_URL" (upcase (symbol-name type))) url)
        (org-entry-put (point) "URL" url)))))

;; TODO: There are multiple subroutines which update data
(defun ref-man--update-props-from-assoc (props-alist)
  "Update a heading and property drawer from a PROPS-ALIST.
For the properties of a heading we preserve the non ascii
accents.  The inverse should happen when parsing a bib entry from
the heading."
  (org-insert-property-drawer)
  (seq-do (lambda (x)
            (pcase-let ((`(,a . ,b) x))
              (pcase a
                ("ABSTRACT" nil)
                ("AUTHOR" (org-set-property a (ref-man--invert-accents (string-trim b))))
                ("URL" (ref-man-org-add-url-property (string-trim b)))
                (_ (org-entry-put (point) a (string-trim b))))))
          props-alist)
  (let ((key (ref-man-parse-properties-for-bib-key)))
    (unless key
      ;; FIXME: This function should filter the user read key
      ;; ref-man--build-bib-key-from-plist
      (setq key (read-from-minibuffer (format "Could not parse key:\nauthor: %s\ntitle: %s\nyear: %s"
                                              (org-entry-get (point) "AUTHOR")
                                              (org-entry-get (point) "TITLE")
                                              (org-entry-get (point) "YEAR")))))
    (org-set-property "CUSTOM_ID" key))
  (when (string-empty-p (org-get-heading t t t t))
    (unless (org-at-heading-p)
      (outline-previous-heading))
    (end-of-line)
    (insert (a-get props-alist "TITLE")))
  (when (and (not (string= (org-get-heading t t t t) (a-get props-alist "TITLE")))
             (or ref-man-always-update-heading-if-different
                 (y-or-n-p "Heading and Title differ.  Update? ")))
    (org-edit-headline (a-get props-alist "TITLE"))))

(defun ref-man-org-add-list-from-assoc (props-alist keys)
  "Add list items to a `ref-man' entry from a properties alist.

List items can be venue, abstract, author etc."
  (seq-do
   (lambda (key)
     (when (assoc key props-alist)
       (ref-man-org-insert-prop-list-item `(,(downcase (if (symbolp key) (symbol-name key) key))
                                            .
                                            ,(format "%s" (a-get props-alist key)))
                                          (current-buffer) t)))
   (reverse keys)))

;; START: Org Semantic Scholar Functions

(defvar ref-man-org-ss-data-insert-prefix-behaviour '(((4) . update) ((16) . display)))

(defun ref-man-update-ss-data-on-disk ()
  "Fetch and force update Semantic Scholar data for org entry at point.
See `ref-man-fetch-ss-data-for-entry' for details."
  (interactive)
  (ref-man-ss-fetch-paper-details (ref-man-org-get-ss-id) t))

(defun ref-man-org-fetch-ss-data-for-entry (&optional update update-on-disk)
  "Fetch the Semantic Scholar data for org entry at point.

With one prefix \\[universal-argument] update the data on disk also.

See `ref-man-ss-fetch-paper-details' for details on how it's stored and
fetched."
  (let ((ss-data (ref-man-ss-fetch-paper-details (ref-man-org-get-ss-id) update-on-disk)))
    (if (not ss-data)
        (message "[ref-man] Could not retrieve Semantic Scholar data for entry")
      (if update
          (ref-man--org-bibtex-write-ref-from-ss-ref ss-data nil t)
        (unless (org-entry-get (point) "PAPERID")
          (org-entry-put (point) "PAPERID" (a-get ss-data 'paperId)))
        (message "[ref-man] Fetched Semantic Scholar Data")))))

(defun ref-man-org-fetch-ss-data-subtree ()
  "Fetch and store SS data for org entries.

With a prefix arg, fetch for all headings in buffer.  Default is
to fetch for the subtree under point."
  (interactive)
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (unless current-prefix-arg
        (org-narrow-to-subtree))
      (while (outline-next-heading)
        (let* ((props (org-entry-properties))
               (title (cdr (assoc "TITLE" props)))
               (author (cdr (assoc "AUTHOR" props)))
               (year (cdr (assoc "YEAR" props))))
          (if (and title author year)
              (message "Skipping entry %s" title)
            (ref-man-org-update-entry-with-ss-data 1)))))))

(defun ref-man-org-update-entry-with-ss-data (&optional arg)
  "Update current entry with Semantic Scholar Data.

Optional ARG is for interactive use.

The data is fetched if required from `semanticscholar.org' and
the properties are updated.  Citations and references are not
displayed or inserted.  See `ref-man-org-fetch-ss-data-for-entry' for
details.

With a single prefix \\[universal-argument] fetch the data from
`semanticscholar.org' even if it's present in cache."
  (interactive "p")
  (util/with-check-mode
   'org-mode "[ref-man]"
   (pcase arg
     (1 (ref-man-org-fetch-ss-data-for-entry t))
     (_ (ref-man-org-fetch-ss-data-for-entry t t)))))

(defface ref-man-org-load-more-face
  '((default :weight bold)
    (((class color)) :foreground "blue"))
  "Face used to show number of citations in Semantic Scholar
Display buffer.")

(defun ref-man-org-maybe-insert-cite-count (total-cites &optional num-cites)
  "Insert TOTAL-CITES and current NUM-CITES for displayed SS data."
  (let* ((show-citations (save-excursion
                           (goto-char (point-max))
                           (re-search-backward (regexp-quote "** citations") nil t)))
         (num-cites (when show-citations
                      (or num-cites
                          (save-excursion
                            (goto-char show-citations)
                            (util/org-count-subtree-children))))))
    (when (and num-cites (< num-cites total-cites))
      (goto-char (point-max))
      (when (not (bolp))
        (insert "\n")
        (forward-line))
      (insert (format "[....Showing %s out of %s entries....]" num-cites total-cites))
      (let* ((beg (point-at-bol))
             (end  (point-at-eol))
             (overlay (make-overlay beg end)))
        (add-text-properties beg end '(read-only t))
        (overlay-put overlay 'ref-man-org-load-more-ov 'read-only)
        (overlay-put overlay 'face 'ref-man-org-load-more-face)))))

(defun ref-man-org-fetch-more-citations (&optional count all)
  "Insert Semantic Scholar Data for org entry at point.

The data is fetched if required from `semanticscholar.org' and
inserted in the current subtree.  If `current-prefix-arg' is
non-nil then insert only references.  Useful when the number of
citations are really high, e.g. above 1000. Default is to insert
both References and Citations.

See `ref-man-org-fetch-ss-data-for-entry' for details."
  (interactive "p")
  (pcase-let* ((count (max count 100))
               (`(,offset ,total)
                (mapcar #'string-to-number
                 (save-excursion
                   (goto-char (point-max))
                   (re-search-backward "\\[....Showing \\([0-9]+\\) out of \\([0-9]+\\) entries....]")
                   (list (match-string 1) (match-string 2)))))
               (ssid (save-excursion
                       (goto-char (point-min))
                       (org-entry-get (point) "PAPERID")))
               (data (ref-man-ss-fetch-paper-citations
                      ssid `((offset . ,offset) (count . ,(if all total count))))))
    (goto-char (point-max))
    (re-search-backward "\\[....Showing \\([0-9]+\\) out of [0-9]+ entries....]")
    (let ((inhibit-read-only t))
      (remove-text-properties (match-beginning 0) (match-end 0) '(read-only t))
      (delete-region (match-beginning 0) (match-end 0))
      (ov-clear))
    (outline-back-to-heading)
    (seq-do (lambda (x)
              (ref-man--org-bibtex-write-ref-from-ss-ref x t nil t))
            data)
    (ref-man-org-maybe-insert-cite-count total (+ offset (if all total count)))))

(defun ref-man-org-fetch-all-citations ()
  (interactive)
  (ref-man-org-fetch-more-citations 0 t))

(defun ref-man-org-insert-ss-data ()
  "Insert Semantic Scholar Data for org entry at point.

The data is fetched if required from `semanticscholar.org' and
inserted in the current subtree.  If `current-prefix-arg' is
non-nil then insert only references.  Useful when the number of
citations are really high, e.g. above 1000. Default is to insert
both References and Citations.

See `ref-man-org-fetch-ss-data-for-entry' for details."
  (interactive)
  (let ((only (pcase current-prefix-arg
                ('(4) 'refs)
                ('(16) 'cites)
                (_ nil)))
        (ss-data (ref-man-ss-fetch-paper-details (ref-man-org-get-ss-id))))
    (pcase-let ((`(,_ ,end ,_) (util/org-heading-and-body-bounds)))
      (goto-char end)
      (org-insert-heading)
      (ref-man-org-insert-ss-data-subr ss-data (current-buffer) nil t only t t))))

(defun ref-man-filter-org-ss--min-max-num (x v)
  (and (> (string-to-number x) (string-to-number (car v)))
       (if (cdr v) (< (string-to-number x) (string-to-number (cdr v))) t)))

(defun ref-man-org-filter-ss--regexp (x v &rest _)
  (let ((case-fold-search t))
    (string-match-p x (car v))))

(defun ref-man-org-filter-ss--read-min-max (prop)
  (let ((v (split-string (read-from-minibuffer
                          (format "Enter [min]-[max] value for %s: " prop))
                         "-")))
    `(,(car v) . ,(nth 1 v))))

(defun ref-man-org-filter-ss--read-regexp (prop)
  (read-from-minibuffer
   (format "Enter regexp for %s: " prop)))

(defun ref-man-org-filter-ss--read-words-list (prop)
  (split-string
   (read-from-minibuffer
    (format "Enter space separated list of words for %s: " prop))))

(defun ref-man-org-filter-ss-display ()
  "Filter Semantic Scholar Data based on one of the properties.

Four properties CITATIONCOUNT, INFLUENTIALCITATIONCOUNT, YEAR,
VENUE are currently supported.

The function can't support more complex filters without hydras,
as such one can work around that like this:

(with-current-buffer some-buffer
  (let ((cc '(\"80\" . \"100\"))
        (yy '(\"2020\" . \"2022\")))
    (org-scan-tags 'sparse-tree (lambda (todo tags-list level)
                                  (setq org-cached-props nil)
                                  (let ((cite-count (org-cached-entry-get nil \"CITATIONCOUNT\"))
                                        (year (org-cached-entry-get nil \"YEAR\")))
                                    (when (and cite-count year)
                                      (and (funcall 'ref-man-filter-ss--min-max-num cite-count cc)
                                           (funcall 'ref-man-filter-ss--min-max-num year yy)))))
                   nil)))
"
  (interactive)
  (pcase-let* ((c (read-char-exclusive "Filter Property:  [c]itation_count  [i]nfluential_citation_count [y]ear  [v]enue (regexp)"))
               (`(,prop ,fn) (pcase c
                               (?c (list "CITATIONCOUNT" 'ref-man-filter-ss--min-max-num))
                               (?i (list "INFLUENTIALCITATIONCOUNT" 'ref-man-filter-ss--min-max-num))
                               (?y (list "YEAR" 'ref-man-filter-ss--min-max-num))
                               (?v (list "VENUE" 'ref-man-filter-ss--regexp))))
               (bounds (pcase c
                         ((or ?c ?i ?y)
                          (ref-man-org-filter-ss--read-min-max prop))
                         (_ (ref-man-org-filter-ss--read-regexp prop)))))
    (org-scan-tags 'sparse-tree (lambda (todo tags-list level)
                                  (setq org-cached-props nil)
                                  (let ((prop-val (org-cached-entry-get nil prop)))
                                    (when prop-val
                                      (funcall fn prop-val bounds))))
                   nil)))

(defconst ref-man-org-filter-key-alist
  '((?y . "year")
    (?c . "citationcount")
    (?i . "influentialcitationcount")
    (?t . "title")
    (?v . "venue"))
  "Convenience alist of key to filter names.

See `ref-man-org-update-filtered-ss-citations'.")

(defun ref-man-org-filter-conversion-vals (f)
  "Convenience function for conversion of user input for filters.

F is an element of filters as input by user.  See
`ref-man-org-update-filtered-ss-citations' for how it's used."
  (pcase (car f)
    ((and (or 'year 'citationcount 'influentialcitationcount) c)
     `(,c
       (min . ,(if (numberp (cadr f)) (cadr f) (string-to-number (cadr f))))
       (max . ,(if (numberp (caddr f)) (caddr f) (string-to-number (caddr f))))))
    ((and 'venue c) `(venue . ((venues . ,(cdr f)))))
    ((and 'title c) `(title_re  ,(cadr f)))
    (_ (user-error "Uknown filter %s" (car f)))))

(defun ref-man-org-filter-conversion (f)
  "Convenience function for conversion of user input for filters.

F is an element of filters as input by user.  See
`ref-man-org-update-filtered-ss-citations' for how it's used."
  (pcase (car f)
    ((and (or ?c ?i ?y) c)
     `(,(a-get ref-man-org-filter-key-alist c)
       (min . ,(string-to-number (cadr f)))
       (max . ,(and (cddr f) (string-to-number (cddr f))))))
    ((and ?v c) `((venues . ,(cdr f))))
    ((and ?t c) `((title_re . ,(cadr f))))
    (_ (user-error "Uknown filter %s" (car f)))))

(defun ref-man-org-update-filtered-subr (data &optional abstract)
  (if (length data)
      (progn
        (goto-char (point-min))
        (re-search-forward (regexp-quote "** citations") nil t)
        (let ((inhibit-read-only t))
          (remove-text-properties (point) (point-max) '(read-only t))
          (delete-region (point) (point-max))
          (ov-clear))
        (outline-back-to-heading)
        (org-insert-heading-after-current)
        (org-do-demote)
        (seq-do-indexed (lambda (x i)
                          (ref-man--org-bibtex-write-ref-from-ss-ref x t (= i 0) (not abstract)))
                        data)
        (message "Inserted %d citations" (length data)))
    (message "No citations found for those filters")))

(defun ref-man-org-update-filtered-ss-citations (&optional count)
  "Filter and update the citations subtree of ref-man buffer.

Filters are applied by the python service and only the citations
subtree is modified.

Current allowed filters are defined in `ref-man-org-filter-key-alist'.

See also `ref-man-org-filter-ss-display' which makes a sparse
tree according to requirements."
  (interactive)
  (let ((ssid (save-excursion
                (goto-char (point-min))
                (org-entry-get (point) "PAPERID")))
        (fchars (string-to-list (read-from-minibuffer
                                 "Enter the characters for filters: {y-year,c-cite_count} etc.: ")))
        (count (or count 20))
        filters)
    (unless (= (length (-uniq fchars)) (length fchars))
      (user-error "Filters should not repeat"))
    (seq-do (lambda (c)
              (pcase c
                ((or ?c ?i ?y)
                 (push `(,c . ,(ref-man-org-filter-ss--read-min-max
                                (a-get ref-man-org-filter-key-alist c)))
                       filters))
                (?t (push `(,c . ,(ref-man-org-filter-ss--read-regexp
                                   (a-get ref-man-org-filter-key-alist c)))
                          filters))
                (?v (push `(,c . ,(ref-man-org-filter-ss--read-words-list
                                   (a-get ref-man-org-filter-key-alist c)))
                          filters))))
            fchars)
    (setq filters (mapcar #'ref-man-org-filter-conversion filters))
    (let ((data (ref-man-ss-fetch-paper-citations ssid `((count . ,count)) `((filters . ,filters)))))
      (ref-man-org-update-filtered-subr data))))

;; TODO: The display buffer should happen in background in an async process
(defun ref-man-org-display-ss-data (&optional pref-arg)
  "Display Semantic Scholar Data for an org entry.

If the cursor is over a an internal org link for which SS data
exists goto that heading else, the current org entry.

The data is fetched if required from `semanticscholar.org' and
displayed in a new org buffer named \"*Semantic Scholar*\".

With a single \\[universal-argument] prefix, display only
references.  With two \\[universal-argument] prefix, display
only citations.  Useful when the number of citations are really
high, e.g. above 1000.

Default is to display both References and
Citations.  See `ref-man-org-fetch-ss-data-for-entry' for details."
  (interactive "p")
  (let ((link (let (current-prefix-arg)
                (util/org-link-get-target-for-internal)))
        (only (pcase pref-arg
                (4 'refs)
                (16 'cites)
                (_ nil))))
    (save-excursion
      (let* ((ssid (if link
                       (let* ((buf (find-file-noselect (plist-get link :file)))
                              (pt (plist-get link :point)))
                         (save-excursion
                           (with-current-buffer buf
                             (goto-char pt)
                             (or (ref-man-org-get-ss-id)
                                 (user-error "No SSID found for entry")))))
                     (ref-man-org-get-ss-id)))
             (ss-data (ref-man-ss-fetch-paper-details ssid)))
        (if ss-data
            (progn
              (message "[ref-man] Preparing Semantic Scholar Data for display")
              (let* ((heading (a-get ss-data 'title))
                     (buf (get-buffer-create (format "*Semantic Scholar/%s*" heading)))
                     (win (util/get-or-create-window-on-side)))
                (set-window-buffer win buf)
                (with-current-buffer buf
                  (let ((inhibit-read-only t))
                    (set-text-properties (point-min) (point-max) nil))
                  (erase-buffer)
                  (org-mode)
                  (org-insert-heading)
                  (insert heading)
                  (org-insert-property-drawer)
                  (org-entry-put (point) "PAPERID" (a-get ss-data 'paperId))
                  (forward-line)
                  (ref-man-org-insert-ss-data-subr ss-data buf nil t only)
                  (ref-man-org-maybe-insert-cite-count (a-get ss-data 'citationCount))
                  (message "[ref-man] Displaying Semantic Scholar Data"))
                (pop-to-buffer buf)))
          (user-error "Got no data to display"))))))

(defun ref-man-org-search-semantic-scholar (search-string &optional insert-first args)
  "Search Semantic Scholar for SEARCH-STRING.

Optional ARGS should be an alist of which converts to valid json.
The json object should correspond to Semantic Scholar Search api
keywords, e.g.: \"{'fieldsOfStudy': ['computer-science'],
'yearFilter': {'max': 1995, 'min': 1990}}\" or, '((fieldsOfStudy
. (computer-science)) (yearFilter (max . 1995) (min . 1990)))

As of now, by default INSERT-FIRST is set to t later in the code as
pagination of results isn't supported yet."
  (interactive (list (let* ((ss (if (eq major-mode 'org-mode)
                                    (substring-no-properties (org-get-heading t t t t)) ""))
                            (prompt (if (string-empty-p ss)
                                        "Search String: "
                                      (format "Search String (default %s): " ss))))
                       (read-from-minibuffer prompt ss nil nil nil ss))))
  (if (string-empty-p search-string)
      (user-error "Empty Search String")
    ;; TODO: fetch page from python server and can select which entry to insert.
    ;;       Should use jinja or lisp template
    (let* ((pref-arg current-prefix-arg)
           (fetch-func (if (and  pref-arg (equal pref-arg '(4)))
                           'ref-man-ss-search
                         'ref-man-ss-get-results-search-semantic-scholar))
           (results (funcall fetch-func search-string args)))
      (cond ((> (length results) 0)
             (let* ((idx (if (and insert-first (length results)) 0
                           (let* ((entries (pcase fetch-func
                                             ('ref-man-ss-get-results-search-semantic-scholar
                                              (ref-man-ss-search-results-to-ido-prompts results))
                                             ('ref-man-ss-search
                                              (ref-man-ss-graph-search-results-to-ido-prompts results))))
                                  (entry (ido-completing-read "Entry to insert: " entries)))
                             (- (string-to-number (car (split-string entry ":" t))) 1)))))
               (pcase fetch-func
                 ('ref-man-ss-get-results-search-semantic-scholar
                  (let ((result (ref-man-ss-parse-search-result (aref results idx))))
                    (ref-man--update-props-from-assoc result)
                    (ref-man-org-add-list-from-assoc
                     result '("ABSTRACT" "AUTHOR" "VENUE" "YEAR"))))
                 ('ref-man-ss-search
                  (ref-man--org-bibtex-write-ref-from-ss-ref (aref results idx) nil t)))))
            ((a-get result 'matchedPresentations)
             (let* ((entries (ref-man-ss-search-presentations-to-ido-prompts
                              (a-get result 'matchedPresentations)))
                    (entry (ido-completing-read "Entry to insert: " entries))
                    (id (a-get (aref (a-get result 'matchedPresentations)
                                     (- (string-to-number (car (split-string entry ":" t))) 1))
                               'id)))
               (ref-man-org-safe-update-prop "PAPERID" id)))
            (t (message "[ref-man] Nothing from Semantic Scholar"))))))

;; END: Org Semantic Scholar Functions

(defun ref-man-org-safe-update-prop (prop val &optional pt)
  "Safely update org heading property PROP with VAL.

With optional PT goto that point first."
  (when (and prop val)
    (let ((test (or (not (org-entry-get (or pt (point)) prop))
                    (and (org-entry-get (or pt (point)) prop)
                         (y-or-n-p (format "Property %s exists.  Update Anyway? " prop))))))
      (when test
        (org-entry-put (or pt (point)) prop val)))))

(defun ref-man-org-maybe-set-arxiv-id (pt url)
  "Set ARXIVID If URL is an arxiv url at point PT."
  (let ((id (ref-man-url-to-arxiv-id url)))
    (when id
      (org-entry-put pt "ARXIVID" id))))

(defun ref-man-import-pdf-url-to-org-buffer (&optional url web-buf org-buf pt)
  "Import a pdf url, from a `ref-man-web' buffer to an org buffer `org-buf'.
Optional arguments URL, WEB-BUF, ORG-BUF and PT if not given are
inferred."
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
              (y-or-n-p (format "%s is not a valid PDF url.  Add anyway? " pdf-url)))
          (with-current-buffer org-buf
            (org-entry-put org-point "PDF_URL" pdf-url)
            (ref-man-org-maybe-set-arxiv-id org-point pdf-url))
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

(defun ref-man-org-get-buf-and-point (&optional org-buf org-point)
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
:link-text LINK-TEXT -- the text of the link,
                        inserted as title/heading
:metadata METADATA -- From Google Scholar,
                      it's the line immediately after the title.

Optional non-nil argument CURRENT specifies whether to update the
current headline.  Default is to insert a subheading."
  (let*
      ;; ((org-buf (or ref-man--org-gscholar-launch-buffer
      ;;                 (let ((org-links-file-name (file-name-nondirectory ref-man-org-links-file-path)))
      ;;                   (if (get-buffer org-links-file-name) (get-buffer org-links-file-name)
      ;;                     (find-file-noselect ref-man-org-links-file-path)))))
      ;;    (org-point (or (and ref-man--org-gscholar-launch-buffer ref-man--org-gscholar-launch-point)
      ;;                   (with-current-buffer org-buf (point)))))
      ((org-data (ref-man-org-get-buf-and-point))
       (org-buf (plist-get org-data :org-buf))
       ;; NOTE: Not used
       ;; (org-point (plist-get org-data :org-point))
       (link (a-get args 'link))
       (title (a-get args 'title))
       (metadata (a-get args 'metadata))
       (authors (a-get args 'authors))
       (date (a-get args 'date))
       (abstract (a-get args 'abstract)))
    (unless metadata
      (setq metadata (-filter 'cdr `((authors . ,authors) (date . ,date) (abstract . ,abstract)))))
    (with-current-buffer org-buf
      (ref-man-org-insert-link-as-headline org-buf link title metadata current))))

(defun ref-man--download-pdf-redirect-new (callback url &optional args)
  "Redirect to Fetch PDF from URL.

Call function CALLBACK after fetching.

Optional ARGS are passed on to the callback with URL as a
list (url args)."
  (message "[ref-man] Fetching PDF from %s" url)
  (let ((url (ref-man-url-maybe-proxy url)))
    (url-retrieve url callback (list url args))))

(defun ref-man--download-pdf-redirect (callback url &optional point)
  "Redirect to Fetch PDF from URL.

Call function CALLBACK after fetching.

Optional POINT is passed on to the callback with URL as a
list (url point)."
  (message "[ref-man] Fetching PDF from %s" url)
  (let ((url (ref-man-url-maybe-proxy url)))
    (if point
        (url-retrieve url callback (list url point))
      (url-retrieve url callback (list url)))))

;; FIXME: If not file and not args then visit with EWW? What does ARGS do?
;; FIXME: args are not optional here
(defun ref-man--fetch-from-pdf-url-new (url &optional args)
  "Fetch pdf file if possible, from URL.

Check if the file exists on disk first.

Call function `ref-man--download-pdf-redirect-new' with the
callback `ref-man--eww-pdf-download-callback-store-new' and
arguments optional ARGS.  Update the PDF_FILE property for the
org entry after downloading."
  (if (and url (not (string-empty-p url)))
      (let ((file (ref-man-files-check-pdf-file-exists url t))
            (url (ref-man-url-maybe-proxy url))
            (buf (and args (plist-get args :buffer)))
            (pt (and args (plist-get args :point)))
            (heading (and args (plist-get args :heading))))
        (cond ((and file (not args))
               (message "[ref-man] File already exists and no args given."))
              ((and file args (with-current-buffer buf (eq major-mode 'org-mode)))
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
               (when (and buf pt)
                 (with-current-buffer buf
                   (save-excursion
                     (goto-char pt)
                     (org-set-property "PDF_URL" (ref-man-url-maybe-unproxy url)))))
               (ref-man--download-pdf-redirect-new
                #'ref-man--eww-pdf-download-callback-store-new url args))
              ((and (not file) (not args))
               (save-excursion
                 (with-current-buffer ref-man--org-gscholar-launch-buffer
                   (goto-char ref-man--org-gscholar-launch-point)
                   (ref-man--insert-org-pdf-file-property file))))))
    (message "[ref-man] Empty pdf url given")))

;; NOTE: Used only by `ref-man--handle-json-response'
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

;; FIXME: What does this do?
(defun ref-man--update-subtree-list (url status)
  (push (list :url url :point (point)
              :heading (org-link-heading-search-string)
              :status status)
        ref-man--subtree-list))

(defun ref-man-maybe-fetch-pdf-from-cvf (args)
  "Fetch pdf from a CVF url if present.

See `ref-man-url-get-cvf-url' on how the presence of a CVF url is
determined.

ARGS are ignored in this case."
  (let* ((doi (org-entry-get (point) "DOI"))
         (venue (org-entry-get (point) "VENUE"))
         (cvf-entry (ref-man-url-parse-cvf-venue doi venue))
         (url (when cvf-entry
                    (ref-man-url-get-cvf-url (org-entry-get (point) "TITLE")
                                             cvf-entry nil
                                             (org-entry-get (point) "YEAR")))))
    (when url
      (prog1 t (ref-man--fetch-from-pdf-url-new url args)))))

(defun ref-man-maybe-fetch-pdf-from-pdf-url (args)
  "Fetch pdf from pdf-url if exists in :urls property of plist ARGS.
If pdf-url doesn't exist then check if first non-nil `cdr' of
:urls is a downloadable pdf url.  Fetch pdf if possible."
  (let* ((urls (plist-get args :urls))
         (url (or (a-get urls 'pdf-url)
                  (cdr (-first #'cdr urls))))
         (url (and url (ref-man-url-downloadable-pdf-url-p url t))))
    (when url
      (prog1 t (ref-man--fetch-from-pdf-url-new url args)))))

(defun ref-man-maybe-fetch-pdf-after-redirect (args)
  "Fetch pdf from one of the :urls property of plist ARGS after redirect."
  (let ((url (cdr (-first #'cdr (plist-get args :urls)))))
    (ref-man-url-get-pdf-url-according-to-source url #'ref-man--fetch-from-pdf-url-new args)))

(defvar ref-man-fetch-pdf-functions
  '(ref-man-maybe-fetch-pdf-from-pdf-url
    ref-man-maybe-fetch-pdf-from-cvf
    ref-man-maybe-fetch-pdf-after-redirect)
  "Functions to fetch a PDF url and store in an org buffer.

Each of the functions is called with a plist with keys:

:URLS are all avaiable URLs in the org property drawer.
:BUFFER is the buffer from which they were called.
:POINT is the point in buffer.
:HEADING is the heading of the subtree at point.")

(defun ref-man-try-fetch-pdf-from-url (org-buf pt heading urls
                                               &optional retrieve-pdf retrieve-bib
                                               retrieve-title storep)
  "Try and fetch pdf and bib entry from URL.

We Can only retrieve from specific sites.  See
`ref-man-url-get-pdf-link-helper' for a list of supported sites.

ORG-BUF is the org buffer to fetch in.

PT, HEADING, URLS, PDF-URL are `point', org heading, url list and
pdf url in org entry properties respectively.

Optional STOREP specifies whether to store the retrieved data to
org file or not.

If at least one of two optional arguments RETRIEVE-PDF or
RETRIEVE-BIB are non-nil then the corresponding pdf and/or bibtex
entry are/is fetched and stored if the option is given.  Org
buffer to insert the data is set by
`ref-man--org-gscholar-launch-buffer'.

RETRIEVE-TITLE has no effect at the moment."
  (util/with-check-mode
   'org-mode "[ref-man]"
   ;; NOTE: Code to prefetch url
   ;;
   ;;(defvar ref-man--fetched-url-buffers nil
   ;;   "Internal variable to hold fetched (url . buffer) pairs.")
   ;; (defvar ref-man--fetching-url-buffers nil
   ;;   "Internal variable to hold (url . buffer) pairs being fetched right now.")
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
   (when retrieve-pdf
     (let ((maybe-pdf-url (-first #'cdr urls))
           (args (list :urls urls :buffer org-buf :point pt :heading heading)))
       (run-hook-with-args-until-success 'ref-man-fetch-pdf-functions args)
       ;; FIXME: Subtree list needn't exist. Pass org heading (and point)
       ;;        to fetch-from-pdf-url callback
       (when storep
         (ref-man--update-subtree-list maybe-pdf-url "fetching"))))
   (when (and retrieve-bib (not storep)) ; don't try retrieve bib when storep
     ;; NOTE: Code to fetch bib entries
     ;;
     ;; (message "[ref-man] Retrieving bib entry")
     ;; (cond ((and url (ref-man-url-has-bib-url-p url))
     ;;        (let ((bib-url (ref-man--try-get-bib-according-to-source url)))
     ;;          (if bib-url
     ;;              (ref-man-eww--browse-url bib-url nil ref-man--org-gscholar-launch-buffer)
     ;;            (message "[ref-man] No bibtex URL in %s" url))))
     ;;       ((not url)
     ;;        (message "[ref-man] No URL given"))
     ;;       ((and url (not (ref-man-url-has-bib-url-p url)))
     ;;        (message "[ref-man] URL doesn't have a bib URL"))
     ;;       (t (message "[ref-man] Some strange error occured for retrieve-bib in %s. Check" url)))
     (message "[ref-man] Retrieving bib entry not implemented"))
   (when retrieve-title
     ;; NOTE: Not worth it right now.
     (message "[ref-man] Let's see if we can insert heading"))
   (when (and (not retrieve-bib) (not retrieve-pdf))
     (message "[ref-man] Nothing to do"))))

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
  "Try fetch and store pdfs in current subtree."
  (interactive)
  (message "[ref-man] Fetching PDFs in subtree. Wait for completion...")
  (setq ref-man--current-org-buffer (current-buffer))
  (setq ref-man--subtree-list nil)
  (save-restriction
    (org-narrow-to-subtree)
    (org-content t)
    (let ((ref-man--subtree-num-entries 0))
      (while (not (eobp))
        (when (outline-next-heading)
          (cl-incf ref-man--subtree-num-entries)
          (ref-man-org-try-fetch-and-store-pdf t))))))

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
              (ref-man-delete-blank-lines-in-buffer)))))
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

(defun ref-man-check-heading-non-empty-p ()
  "Check if the org heading a non empty string."
  (> (length (string-trim (substring-no-properties (org-get-heading t t t t)))) 0))

(defun ref-man--check-fix-url-property (&optional props)
  "Fix the URL property in the property drawer.

If optional PROPS is given then use those instead of reading from
org entry.

Make sure URL property exists in either property drawer or text
and if no URL could be found return nil.  If no URL property
exists, then first link from entry text is imported into the
property drawer as the URL property."
  (save-excursion
    (let* ((props (or props (org-entry-properties)))
           (url-prop (cdr (assoc "URL" props))))
      ;; Remove the url args. Would be useless to keep
      (when (and url-prop (string-match-p "?.*" url-prop)
                 (not (string-match-p "openreview.net" url-prop))
                 (not (string-match-p "acm.org" url-prop))
                 (not (string-match-p "ieeexplore.ieee.org" url-prop)))
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
            (ref-man-org-add-url-property url))
          (setq url-prop url)))
      url-prop)))

(defun ref-man-parse-properties-for-bib-key (&optional arg)
  "Check if bibtex key can be determined from entry properties."
  (interactive "p")
  (let* ((props (org-entry-properties))
         (title (cdr (assoc "TITLE" props)))
         (author (cdr (assoc "AUTHOR" props)))
         (year (cdr (assoc "YEAR" props))))
    (if (and title author year)
        (condition-case nil
            (let ((key (ref-man--build-bib-key-from-plist
                        (list :title title :author author :year year))))
              (if arg
                  (if current-prefix-arg
                      (org-entry-put (point) "CUSTOM_ID" key)
                    (message "Killed. Use C-u to add to entry")
                    (kill-new key))
                key))
          (error (message "Could not parse as bibtex\ntitle: %s, author: %s, year: %s"
                          title author year)
                 nil))
      (message "Not enough properties to parse as bibtex\ntitle: %s, author: %s, year: %s"
               title author year)
      nil)))

(defun ref-man-org-download-pdf-links-in-notes ()
  "Download PDF links in notes to designated directory.

Notes are any given subtree with a specified storage directory.
The pdfs are downloaded and the file links are marked as `(file here)'"
  (save-excursion
    (save-restriction
      (let ((case-fold-search t)
            links)
        (org-narrow-to-subtree)
        (goto-char (point-min))
        (org-end-of-meta-data)
        (while (re-search-forward "http.+?\\.pdf" nil t)
          (push (list :beg (match-beginning 0)
                      :end (match-end 0)
                      :link (substring-no-properties (match-string 0)))
                links))
        (seq-do (lambda (pl)
                  (let ((link (plist-get pl :link))
                        (beg (plist-get pl :beg))
                        (end (plist-get pl :end))
                        file-link)
                    ;; TODO: push to file links, perhaps parallel
                    (setq file-link (wget-download link))
                    (if file-link
                        (replace-region-contents beg end
                                                 (lambda (contents)
                                                   (concat contents " " file-link)))
                      (user-error "Error downloading link %s" file-link))))
                links)))))

(defun ref-man--check-fix-pdf-file-property ()
  "Check current org entry for PDF_FILE property."
  (let* ((props (org-entry-properties))
         (pdf-file-prop (a-get props "PDF_FILE"))
         (pdf-file (when pdf-file-prop
                     (replace-regexp-in-string "\\[\\|\\]" "" pdf-file-prop))))
    (when (and pdf-file-prop (not (f-exists? pdf-file)))
      (org-delete-property "PDF_FILE"))
    (when (and pdf-file-prop (f-exists? pdf-file))
      pdf-file-prop)))


(defun ref-man--check-fix-ss-url (&optional props)
  "Fix SS_URL properties of current org entry.

Replace all *_URL properties in current org entry with SS_URL if
they're of semanticscholar.org.

Use optional PROPS if given instead of current org entry's
properties."
  (let ((props (or props (org-entry-properties))))
    (seq-do (lambda (url)
              (let ((val (a-get props url)))
                (when (and val
                           (string-match-p "semanticscholar.org" val))
                  (org-entry-put (point) "SS_URL" val)
                  (org-entry-delete (point) url))))
            '("URL" "ALT_URL")))
  (org-entry-get (point) "SS_URL"))

(defun ref-man-fetch-pdf-check-pdf-file (pdf-file urls)
  "Check if PDF-FILE already exists in heading.

After checking the file return message for which url it matches.

URLS is an alist of urls with the type of url as the key and url
as the value."
  (let* ((url-keys '(pdf-url url arxiv-url alt-url ss-url))
         (url (-first (lambda (x) (a-get urls x)) url-keys))
         (url (a-get urls url))
         (pdf-file (if (string-match util/org-file-link-re pdf-file)
                       (match-string 1 pdf-file)
                     pdf-file))
         (pdf-file-exists (f-exists? pdf-file))
         (same-msg (when url
                     (if (string= pdf-file (ref-man-files-filename-from-url url))
                         (s-lex-format "And is the same as URL ${url}")
                       (s-lex-format "But is different from URL ${url}"))))
         (msg (cond ((and pdf-file-exists same-msg)
                     (s-lex-format "[ref-man] PDF already exists. ${same-msg}"))
                    ((and (not pdf-file-exists) same-msg)
                     (s-lex-format "[ref-man] PDF does not exist. ${same-msg}"))
                    (t "[ref-man] No PDF URL here."))))
    msg))

;; TODO: Link (arxiv, ACL, DOI) to SS_IDs for papers also, minimize redundancy
;;       NOTE: Perhaps maintain a python cache for that
;; TODO: I have not incorporated dblp extracted entries as they refer
;;       to DOIs or direct arxiv links. Have to fix that.
;; TODO: If the pdf is extracted from a site like cvf, or neurips
;;       or whatever, the corresponding bibtex should also be
;;       extracted and stored from the website itself.
;; TODO: What if entry exists but the file doesn't?
;; TODO: What if the buffer gets killed before the pdf/bib is fetched?
;; FIXME: When downloading from a redirect from SS-URL we should update PDF-URL
(defun ref-man-org-try-fetch-and-store-pdf (&optional interactivep)
  "Try to fetch and store pdf for current entry in an org buffer.
Optional INTERACTIVEP is to check the `interactive' call."
  (interactive "p")
  (org-insert-property-drawer)
  (if (eq major-mode 'org-mode)
      (let* ((buf (current-buffer))
             (pt (point))
             (heading (org-get-heading t t t t))
             (ss-url-prop (ref-man--check-fix-ss-url))
             (url-prop (ref-man--check-fix-url-property))
             (props (org-entry-properties))
             (pdf-url-prop (a-get props "PDF_URL"))
             ;; CHECK: Why're alt-url and arxiv-url not used?
             (alt-url-prop (a-get props "ALT_URL"))
             (arxiv-url-prop (or (a-get props "ARXIV_URL") (ref-man-url-from-arxiv-id)))
             (ssidtype-id (ref-man-org-get-ss-id))
             (pdf-file (ref-man--check-fix-pdf-file-property))
             (bib-prop (ref-man-parse-properties-for-bib-key))
             (headingp (ref-man-check-heading-non-empty-p))
             (urls `((pdf-url . ,pdf-url-prop)
                     (arxiv-url . ,arxiv-url-prop)
                     (url . ,url-prop)
                     (alt-url . ,alt-url-prop)
                     (ss-url . ,ss-url-prop)))
             retrieve-bib retrieve-pdf retrieve-title
             (msg-str ""))
        (when ssidtype-id
          ;; TODO: if ssidtype-id then check if SS has paper
          )
        ;; TODO: Use `hydra' instead of prefixes
        ;; FIXME: Make sure pdf file exists on disk, else remove prop
        ;; FIXME: Maybe check for :link property from text but MEH!
        ;; NOTE: Not sure about this
        (let ((meh current-prefix-arg))
          (cond ((and meh (equal meh '(4)))   ; update only
                 ;; Do something
                 ))
          )
        ;; CHECK: storep signifies to store the filename in PDF_FILE property?
        ;;
        ;; TODO: arxiv-url and any other alt-urls should be sent separately to the subroutine
        ;;       We can also try them in sequence
        (setq msg-str
              (concat msg-str
                      (cond ((and pdf-file (-any #'cdr urls))
                             (concat (ref-man-fetch-pdf-check-pdf-file pdf-file urls)
                                     (if pdf-file " But there was some URL and file exists!" "")))
                            ((and pdf-file (not (-any #'cdr urls)))
                             "[ref-man] PDF exists, but no URL! What to do?!!")
                            ((and (not pdf-file) (-any #'cdr urls))
                             (setq retrieve-pdf t)
                             "[ref-man] No pdf file in properties. Will fetch PDF from URL")
                            ((and (not pdf-file) (not (-any #'cdr urls)))
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
                      (cond ((and headingp (-any #'cdr urls))
                             (setq retrieve-title nil)
                             "Heading exists!")
                            ((and (not headingp) url-prop)
                             (setq retrieve-title t)
                             "No heading. Will fetch from URL!")
                            ((and (not headingp) (not url-prop))
                             (setq retrieve-title nil)
                             "No heading and no URL. What to do?!!"))))
        (when interactivep
          (message msg-str))
        (when (or retrieve-pdf retrieve-bib)
          (ref-man-try-fetch-pdf-from-url buf pt heading urls
                                          retrieve-pdf retrieve-bib retrieve-title
                                          interactivep)))
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

(defun ref-man-org-get-link-peek-info ()
  "Get info from properties for org link at point.

Only works for internal org links."
  (let* ((link (util/org-link-get-target-for-internal))
         (path (when link (plist-get link :file)))
         (buf (when path (find-file-noselect path)))
         (pt (when path (plist-get link :point)))
         (props (when (and buf pt)
                  (with-current-buffer buf
                    (org-entry-properties pt)))))
    (string-join (reverse
                  (-keep (lambda (x)
                           (when (member (car x)
                                         '("TITLE" "AUTHOR" "JOURNAL" "VENUE"
                                           "YEAR" "CITATIONCOUNT" "NUMCITEDBY" "BOOKTITLE"))
                             (format "%s: %s" (capitalize (car x)) (cdr x))))
                         props))
                 "\n\n")))

(defvar ref-man-peek-fg-color nil
  "Foreground color for peeking functions.")
(defvar ref-man-peek-bg-color nil
  "Background color for peeking functions.")
(defvar ref-man-peek-margin-right 40
  "Margin at the right of cursor to show tooltip.")
(defvar ref-man-peek-margin-below 5
  "Margin below the cursor to show tooltip.")
(defun ref-man-org-peek-link-subr (_win _pos _motion)
  "Show information about org link under cursor.
Adapted somewhat from `company-quickhelp--show'."
  (let* ((doc (ref-man-org-get-link-peek-info))
         (width 80)
         (timeout 300)
         (margin-right ref-man-peek-margin-right)
         (margin-below ref-man-peek-margin-below) ; (if (< (or (overlay-get ovl 'company-height) 10) 0) 0 (frame-char-height))
         (x-gtk-use-system-tooltips nil)
         (fg-bg `(,ref-man-peek-fg-color
                  . ,ref-man-peek-bg-color))
         (pos (point)))
    (pos-tip-show doc fg-bg pos nil timeout width nil
                  margin-right margin-below)
    ;; TODO: MAYBE add text properties later
    ;; (if company-quickhelp-use-propertized-text
    ;;     (let* ((frame (window-frame (selected-window)))
    ;;            (max-width (pos-tip-x-display-width frame))
    ;;            (max-height (pos-tip-x-display-height frame))
    ;;            (w-h (pos-tip-string-width-height doc)))
    ;;       (cond
    ;;        ((> (car w-h) width)
    ;;         (setq doc (pos-tip-fill-string doc width nil nil nil max-height)
    ;;               w-h (pos-tip-string-width-height doc)))
    ;;        ((or (> (car w-h) max-width)
    ;;             (> (cdr w-h) max-height))
    ;;         (setq doc (pos-tip-truncate-string doc max-width max-height)
    ;;               w-h (pos-tip-string-width-height doc))))
    ;;       (pos-tip-show-no-propertize doc fg-bg pos nil timeout
    ;;                                   (pos-tip-tooltip-width (car w-h) (frame-char-width frame))
    ;;                                   (pos-tip-tooltip-height (cdr w-h) (frame-char-height frame) frame)
    ;;                                   nil (+ overlay-width overlay-position) dy))
    ;;   (pos-tip-show doc fg-bg pos nil timeout width nil
    ;;               (+ overlay-width overlay-position) dy))
    ))

(defun ref-man-org-peek-link (&optional _arg)
  "Preview org entry's properties in a `pos-tip'."
  (interactive "p")
  (let ((el (org-element-context)))
    (pcase (org-element-type el)
      ('link (ref-man-org-peek-link-subr nil nil nil))
      (_ (message "Nothing to peek here")))))

;; NOTE: Was thinking of putting sensor function like this on all link but the
;;       motion stutters with that. Let's keep it interactive

;; (let* ((el (org-element-context))
;;          (beg (plist-get (cadr el) :begin))
;;          (end (plist-get (cadr el) :end)))
;;     (put-text-property beg end 'cursor-sensor-functions (list #'ref-man-org-peek-link-subr))
;;     )

(defun ref-man-eww-download-pdf (url &optional view)
  "Download the pdf file from the URL and optionally VIEW it.
Also update the PDF-FILE property in org heading drawer if the
web buffer was called from an org buffer."
  (interactive)
  (let ((file (ref-man-files-check-pdf-file-exists url t))
        (url (ref-man-url-maybe-proxy url)))
    (if file
        (if (y-or-n-p "File exists.  Download again? ")
            (progn (message "Downloading %s" url)
                   (url-retrieve url #'ref-man--eww-pdf-download-callback (list url view t))) ; retrieve and view
          (when view (find-file-other-window file))) ; else view
      (message "Downloading %s" url)
      (url-retrieve url #'ref-man--eww-pdf-download-callback (list url view)))))

;; called from an org buffer
;; when to set ref-man--org-gscholar-launch-buffer nil?
;;
;; when gscholar is called from any other buffer than previous
;; ref-man--org-gscholar-launch-point
(defun ref-man-org-search-heading-on-gscholar-with-eww ()
  "Search for the current heading in google scholar with eww.

Store the buffer and the position from where it was called."
  (interactive)
  (if (eq major-mode 'org-mode)
      (progn
        (setq ref-man--org-gscholar-launch-point (point)) ; must be at heading?
        (setq ref-man--org-gscholar-launch-buffer (current-buffer))
        (save-excursion
          (let ((query-string (replace-regexp-in-string ":\\|/" " "
                               (substring-no-properties (org-get-heading t t t t)))))
            (ref-man-web-gscholar query-string))))
    (message "[ref-man] Not in org-mode") nil))
(make-obsolete 'ref-man-org-search-heading-on-gscholar-with-eww
               'ref-man-web-search "")

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
  "Create or insert an org heading with for a given PDF FILE.

The heading contains PDF_FILE property with the FILE's path as
link.

The heading is inserted in `ref-man--org-gscholar-launch-buffer'
if it exists, or `ref-man-org-links-file-path'."
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
  "Return non-nil if we're at a bib heading."
  (let ((props (org-entry-properties)))
    (and (org-at-heading-p)
         (or (assoc "TYPE" props) (assoc "BTYPE" props))
         (assoc "CUSTOM_ID" props))))

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
        (message "[ref-man] Trying to insert into org buffer: %s"
                 (buffer-name org-buf))
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
      (message "[ref-man] inserted bib entry into %s" bib-buf))
    ;; (when bib-buf
    ;;   (let* ((temp-bib-file-name (file-name-nondirectory ref-man-temp-bib-file-path))
    ;;          (buf (if (get-buffer temp-bib-file-name) (get-buffer temp-bib-file-name)
    ;;                 (find-file-noselect ref-man-temp-bib-file-path))))
    ;;     (with-current-buffer buf (goto-char (point-min))
    ;;                          (insert buf-string))
    ;;     (message "[ref-man] inserted bib entry into %s" temp-bib-file-name)))
    (when kill
      (kill-new buf-string))))

;; NOTE: I wanted to do recursion also with this. Maybe use helper
(defun ref-man-parse-subtree-to-buffer-as-bibtex (&optional bib-buf)
  "Insert contents of an org-subtree as bibtex entries to an open buffer.

Read buffer BIB-BUF from user."
  (interactive)
  (unless (boundp 'bib-buf)
    (completing-read "Bib buffer: "
                     (mapcar (lambda (x) (format "%s" x)) (buffer-list))))
  (if (org-at-heading-p)
      (let (heading-has-children)
        (when heading-has-children
          ;; TODO:
          ;; Import heading to bib buffer
          ;; Descend into subtree
          ;; On all children do recursive call to self
          ;; NOTE: Actually it should just be:
          ;; (outline-next-heading) (ref-man-org-bibtex-read-from-headline)
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
