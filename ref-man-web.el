;;; ref-man-web.el --- Web components with `eww' and proxies for `ref-man'. ;;; -*- lexical-binding: t; -*-

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
;; Web components include all the web browsing and associated functions and
;; commands for `ref-man'.  We primarily use `eww' with custom keymaps and hooks
;; for our use, but at times when Google complains about lack of javascript we
;; fetch everything through a headless chrome instance.

;;; Code:

(require 'dash)
(require 'ref-man-url)
(require 'ref-man-files)

;; FIXME: All this eww mode hook should be separate
(defun ref-man-web-eww-mode-hook ()
  "`ref-man' `eww' mode hook."
  (bind-key "b" 'ref-man-eww-keypress-b eww-mode-map)
  (bind-key "b" 'ref-man-eww-keypress-b eww-link-keymap)
  (bind-key "i" 'ref-man-eww-keypress-i eww-mode-map)
  (bind-key "i" 'ref-man-eww-keypress-i eww-link-keymap)
  (bind-key "v" 'ref-man-eww-keypress-v eww-mode-map)
  (bind-key "v" 'ref-man-eww-keypress-v eww-link-keymap)
  (bind-key "d" 'ref-man-eww-keypress-d eww-mode-map)
  (bind-key "d" 'ref-man-eww-keypress-d eww-link-keymap)
  (bind-key "n" 'next-line eww-mode-map)
  (bind-key "p" 'previous-line eww-mode-map)
  (bind-key "]" 'ref-man-eww-next eww-mode-map)
  (bind-key "[" 'ref-man-eww-previous eww-mode-map))

(add-hook 'eww-mode-hook 'ref-man-web-eww-mode-hook)

;; FIXME: these should be moved from here into some config
(global-set-key (kbd "C-c e e") 'eww)
(global-set-key (kbd "C-c e g") 'ref-man-web-gscholar)
(setq browse-url-browser-function 'eww-browse-url)
(defvar ref-man--gscholar-launch-buffer-list
  nil
  "List of org buffers from where launched.")
(make-obsolete-variable 'ref-man--gscholar-launch-buffer-list nil "ref-man 0.3.0")

(declare-function ref-man-files-check-pdf-file-exists "ref-man-files")
(declare-function ref-man-org-import-link "ref-man-core")
(declare-function ref-man-parse-bibtex "ref-man-core")
(declare-function ref-man-eww-download-pdf "ref-man-core")
(declare-function ref-man--sanitize-org-entry "ref-man-core")
(declare-function ref-man-org-clear-property-drawer "ref-man-core")
(declare-function ref-man-org-bibtex-convert-bib-to-property "ref-man-core")


;; Defined in `ref-man-core'
(defvar ref-man--eww-import-link)
(defvar ref-man--org-gscholar-launch-buffer)
(defvar ref-man--org-gscholar-launch-point)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ref-man-web-on-gscholar-page-p ()
  "Check if we're on a Google Scholar page."
  (and (eq major-mode 'eww-mode)
       (string-match-p "scholar\\.google\\.com" (plist-get eww-data :url))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START eww navigation and keymap ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use gscholar specific bindings only on google scholar pages
(defun ref-man-eww-previous ()
  "Goto previous page on Google Scholar."
  (interactive)
  (let ((url (with-current-buffer (get-buffer "*eww*") (plist-get eww-data :url))))
    (if (string-match-p "scholar\\.google\\.com" url)
        (catch 'retval
          (save-excursion
            (goto-char (point-min))
            (while (search-forward "Previous")
              (let ((url (get-text-property (- (point) 1) 'shr-url)))
                ;; (if (string-match-p "scholar.*start=" url)
                ;;     (progn (eww-browse-url url) (message "Going to Previous Page")
                ;;            (throw 'retval t)))
                (when (string-match-p "scholar.*start=" url)
                  (eww-browse-url url) (message "Going to Previous Page")
                  (throw 'retval t))))))
      (eww-previous-url))))

(defun ref-man-eww-next ()
  "Goto previous page on Google Scholar."
  (interactive)
  (let ((url (with-current-buffer (get-buffer "*eww*") (plist-get eww-data :url))))
    (if (string-match-p "scholar\\.google\\.com" url)
        (catch 'retval
          (save-excursion
            (goto-char (point-min))
            (while (search-forward "Next")
              (let ((url (get-text-property (- (point) 1) 'shr-url)))
                (when (and url (string-match-p "scholar.*start=" url))
                  (eww-browse-url url) (message "Going to Next Page")
                  (throw 'retval t))))))
                ;; (if (and url (string-match-p "scholar.*start=" url))
                ;;     (progn (eww-browse-url url) (message "Going to Next Page")
                ;;            (throw 'retval t)))
      (eww-next-url))))

;; CHECK: import link to org-buffer doesn't attach the pdf file automatically
;;       if it's already downloaded.
;;       Same should be there for import first and download later.
;;       However, link to the article and link to pdf may not be the same in general
;; FIXME: This should be integrated with *-chrome functions
(defun ref-man-eww-keypress-i ()
  "Import the Google Scholar entry at point if on Google Scholar.
Default action for key press i otherwise."
  (interactive)
  (let ((url (with-current-buffer (get-buffer "*eww*") (plist-get eww-data :url))))
    (if (string-match-p "scholar\\.google\\.com" url) ; Only imports from scholar for now
        (ref-man-org-import-link
         (ref-man-web-extract-import-link-data (current-buffer))
         current-prefix-arg)
      (eww-view-source))))

;; ;; FIXME: Should go back to URL
;; (defun ref-man-eww-keypress-i ()
;;   (interactive)
;;   (let ((url (plist-get eww-data :url)))
;;     (if (string-match-p "scholar\\.google\\.com" url) ; Only imports from scholar for now
;;         (ref-man-web-extract-import-link-data (get-text-property (point) 'shr-url))
;;       (eww-view-source))))

(defun ref-man-eww-keypress-v ()
  "View and download the pdf from url if required url.

Check if the url at point is a pdf url first.  If it is on a url
or on Google Scholar page, then then call
`ref-man-eww-view-and-download-if-required-pdf'.  Defaults to
view-source of the page if any of those can't be applied."
  (interactive)
  ;; assuming already in eww or shr mode
  (let ((url (get-text-property (point) 'shr-url)))
    (if (or (ref-man-web-on-gscholar-page-p) (and url (ref-man-url-downloadable-pdf-url-p url)))
        (ref-man-eww-view-and-download-if-required-pdf url)
      (eww-view-source))))

;; FIXME: Incomplete. It's not used anywhere also
(defun ref-man-eww-keypress-c ()
  "Cycle between pdf (or perhaps other predicated) links in the eww buffer.
Goes to the next pdf link and cycles round if the last link is reached."
  (interactive)
  ;; (if current-prefix-arg)
  (let* ((buf (get-buffer "*eww*"))
         (url (with-current-buffer buf (plist-get eww-data :url)))
         (links (if (string-match-p "\\.arxiv\\.org" url)
                    (ref-man-web-get-all-links buf nil nil "pdf")
                  (ref-man-web-get-all-links buf nil nil "\\.pdf"))))))

(defun ref-man-eww-keypress-b (&optional org-buf)
  "Extract bibtex from the Googel Scholar entry at point.
With a `\\[universal-argument]' input the org buffer ORG-BUF from user
and extract to it.  Else it defaults to
`ref-man--org-gscholar-launch-buffer'."
  (interactive (list (when (and current-prefix-arg (not (boundp 'org-buf)))
                       (completing-read "Org buffer: "
                                        (mapcar (lambda (x) (format "%s" x)) (buffer-list))))))
  (let ((url (with-current-buffer (get-buffer "*eww*") (plist-get eww-data :url))))
    (if (string-match-p "scholar\\.google\\.com" url)
        (ref-man-eww-get-bibtex-from-scholar org-buf)
      (eww-add-bookmark))))
(make-obsolete 'ref-man-eww-keypress-b nil "ref-man 0.3.0")

;; TODO: Fix this thing. Previous pdf link of gscholar may not be the link
;;       corresponding to that document. As in the previous link may be an html
;;       It downloads the previous downloadable pdf link instead in that case.
;; TODO: There should be a uniform interface to all downloads and view requests
;;       so additional downloads aren't done. At present, the file is downloaded
;;       again, which should be the case only with a prefix key
;; TODO: keypress-d from arxiv.org doesn't store the pdf file in properties of
;;       corresponding org file. Maybe it's because when I follow the link from
;;       org-buffer, `ref-man--org-gscholar-launch-buffer' is not set
(defun ref-man-eww-keypress-d ()
  "Download the pdf file according to context.
Check if the url at point is a pdf url first.  If it is on a url
or on Google Scholar page, then then call
`ref-man-eww-view-and-download-if-required-pdf'.  Defaults to
`eww-download' if any of those can't be applied."
  (interactive)
  ;; was here for scholar.google.com checking
  ;; * ((url (with-current-buffer (get-buffer "*eww*") (plist-get eww-data :url)))
  (let ((url (get-text-property (point) 'shr-url)))
    (cond ((ref-man-url-downloadable-pdf-url-p url)
           (ref-man-eww-download-pdf url))
          ((ref-man-web-on-gscholar-page-p)
           (ref-man-eww-download-pdf (ref-man-web-get-previous-pdf-link (current-buffer))))
          (t (message "[ref-man] Nothing to download here")))))

    ;; (and url (ref-man-url-downloadable-pdf-url-p url))
    ;;     (ref-man-eww-download-pdf url)
    ;;     (message "[ref-man] Nothing to download here"))))

    ;; (if (and url (ref-man-url-downloadable-pdf-url-p url))
    ;;     (ref-man-eww-download-pdf url)
    ;;   (ref-man-eww-download-pdf url))
    ;; (eww-download)))
;;;;;;;;;;;;;;;;;;;;;;;;
;; END eww navigation ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START eww utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ref-man--get-non-google-url (buf pdf-url)
  "Get the next url from PDF-URL from an `shr' buffer BUF."
  (let ((all-urls (ref-man-url-filter-non-gscholar
                   (ref-man-web-get-all-links buf t nil nil)))) ; (from eww buffer) from-begin
    (nth (+ (-elem-index pdf-url all-urls) 1) all-urls)))

(defun ref-man-web-get-import-link-data (buf link)
  "Extract LINK, its text and corresponding metadata from an eww buffer BUF."
  (save-excursion
    (setq ref-man--eww-import-link link)
    (when link
        (with-current-buffer buf
          (let*
              ((link-text-begin (progn (when (not (equal link (get-text-property (point) 'shr-url)))
                                         (while (and (not (equal link (get-text-property (point) 'shr-url)))
                                                     (not (bobp)))
                                           (backward-char)))
                                       (while (equal link (get-text-property (point) 'shr-url))
                                         (backward-char))
                                       (forward-char) (point)))
               (link-text-end (progn ; (goto-char link-text-begin)
                                (while (equal link (get-text-property (point) 'shr-url))
                                  (forward-char))
                                (point)))
               (metadata (progn (goto-char link-text-end)
                                (forward-line 2)
                                (buffer-substring-no-properties
                                 (point-at-bol) (point-at-eol))))
               (link-text (replace-regexp-in-string "\n" " "
                           (buffer-substring-no-properties link-text-begin link-text-end))))
            (list :link-text link-text :metadata metadata))))))

(defun ref-man--check-bibtex-string (buf-string)
  "Check BUF-STRING status from Google Scholar bibtex buffer."
  (if buf-string (cond ((string-match-p "systems have detected unusual" buf-string)
                        (message "[ref-man] Scholar is detecting a robot"))
                       ((string-match-p "client does not have permission" buf-string)
                        (message "[ref-man] Scholar doesn't like EWW"))
                       (t buf-string))
    (message "[ref-man] Empty reply from scholar") nil))

(defun ref-man-eww--check-bibtex-buffer-from-scholar ()
  "Check if the Google Scholar bibtex buffer contains valid data."
  (let* ((buf (get-buffer " *scholar-entry*"))
         (buf-string (if buf (with-current-buffer buf (buffer-string))
                       (message "[ref-man] Could not create buffer for scholar entry") nil)))
    (ref-man--check-bibtex-string buf-string)))

(defun ref-man-eww--browse-url (url &optional callback org)
  "Browse URL in background and perform optional CALLBACK.
If CALLBACK is nil defaults to `ref-man-eww--gscholar-parse-bibtex'.
Optional argument ORG is a target org buffer in which to operate."
  (let ((buf (get-buffer-create " *scholar-entry*")))
    (with-current-buffer buf (eww-setup-buffer)
                         (plist-put eww-data :url url)
                         (plist-put eww-data :title "")
                         (eww-update-header-line-format)
                         (let ((inhibit-read-only t))
                           (goto-char (point-min)))
                         (if callback
                             (url-retrieve url callback
                                           (list url (current-buffer) org))
                           (url-retrieve url #'ref-man-eww--gscholar-parse-bibtex
                                         (list url (current-buffer) org))))))

;; (defun ref-man-eww--extract-everything (buf)
;;   "Extract EVERYTHING!"
;;   (ref-man-chrome-extract-bibtex-from-scholar buf) ; extract bibtex to properties
;;   (ref-man-web-extract-import-link-data      ; also extract file link
;;    (ref-man-eww--gscholar-get-previous-non-google-link
;;     buf))
;;   (ref-man-import-pdf-url-to-org-buffer ; also extract pdf url
;;    (ref-man-web-get-previous-pdf-link buf)))

;; FIXED: Fix this! ref-man--org-gscholar-launch-point is used only for
;;        inserting bibtex. The other variable
;;        ref-man--org-gscholar-launch-buffer is used to insert children and
;;        other stuff and is used in other functions - They can cause confusion.
;;
;; TODO: Have changed some of my/org-*gscholar*launch* variables. Should change
;;       the rest.
;;
;; TODO: Why does it save my/bibtex-entry?
;;
;; FIXED: It doesn't store URL while inserting bibtex.  URL Will have to be
;;        fetched by going back and finding the first non-google URL I guess.
;;
;; FIXME: This should not be called with `eww-render' anyway
(defun ref-man-eww--gscholar-parse-bibtex (status url buf org)
  "Fetch and parse the bibtex from Google Scholar.
As it's a `url-retrieve' callback, STATUS is always the first
argument.  URL is fetched url, BUF is the target buffer for URL
and ORG is the target org buffer."
  (eww-render status url nil buf)
  (let ((check-string (ref-man-eww--check-bibtex-buffer-from-scholar))
        (url (ref-man-eww--gscholar-get-previous-non-google-link (get-buffer "*eww*"))))
    (if check-string
        (ref-man-parse-bibtex buf org)
      (message "[ref-man] Could not get entry from scholar"))
    (when buf (kill-buffer buf))))

(defun ref-man-eww--gscholar-get-next-non-google-link (buf)
  "Get the next non Google link from a buffer BUF."
  (ref-man-eww--gscholar-get-non-google-link buf nil))

(defun ref-man-eww--gscholar-get-previous-non-google-link (buf)
  "Get the previous non Google link from a buffer BUF."
  (ref-man-eww--gscholar-get-non-google-link buf t))

(defun ref-man-eww--gscholar-get-non-google-link (buf previous)
  "Fetch a non Google link from a buffer BUF.
BUF is usually a Google Scholar buffer.  If PREVIOUS is non-nil
fetch the previous non Google link, else fetch the next one."
  (save-excursion
    (with-current-buffer buf
      (let ((step (if previous -1 1)))
        (if (and (get-text-property (point) 'shr-url)
                 (ref-man-url-non-gscholar-url-p (get-text-property (point) 'shr-url))) ; (point) has link
            (get-text-property (point) 'shr-url)
          (while (and (not (bobp))
                      (not (eobp))
                      (not (if (get-text-property (point) 'shr-url)
                               (ref-man-url-non-gscholar-url-p (get-text-property (point) 'shr-url)))))
            (forward-char step))
          (get-text-property (point) 'shr-url))))))

(defun ref-man-web-get-previous-pdf-link (buf)
  "Fetch previous pdf url from an `shr' buffer BUF."
  (ref-man-web-get-pdf-link buf t))

(defun ref-man-web-get-next-pdf-link (buf)
  "Fetch next pdf url from an `shr' buffer BUF."
  (ref-man-web-get-pdf-link buf nil))

(defun ref-man-web-get-pdf-link (buf previous)
  "Fetch a pdf url from an `shr' buffer BUF.
With a non-nil PREVIOUS, fetch the previous pdf link else the
next one."
  (save-excursion
    (with-current-buffer buf
      (let ((step (if previous -1 1)))
        (if (and (get-text-property (point) 'shr-url)
                 (ref-man-url-downloadable-pdf-url-p (get-text-property (point) 'shr-url))) ; (point) has link
            (get-text-property (point) 'shr-url)
          (while (and (not (eobp))
                      (not (if (get-text-property (point) 'shr-url)
                               (ref-man-url-downloadable-pdf-url-p (get-text-property (point) 'shr-url)))))
            (forward-char step))
          (get-text-property (point) 'shr-url))))))

;; FIXME: DEPRECATED
;;        Because there are a bunch of modular functions which do this
(defun ref-man-eww--get-gscholar-link-for-import (buf)
  "Import a publication link from a Google Scholar buffer BUF."
  (save-excursion
    (with-current-buffer buf
      (if (and (get-text-property (point) 'shr-url)
               (ref-man-url-non-gscholar-url-p (get-text-property (point) 'shr-url))) ; (point) has link
          (get-text-property (point) 'shr-url)
        (while (and (not (bobp))
                    (not (if (get-text-property (point) 'shr-url)
                             (ref-man-url-non-gscholar-url-p (get-text-property (point) 'shr-url)))))
          (backward-char 1))
        (get-text-property (point) 'shr-url)))))

;; TODO: Currently no TODO attribute is set on the org entry and no
;;       timestamp is marked. Maybe option for that.
;; TODO: Allow import to specified location in a specified org file also
;;       e.g. research/reading/misc.
(defun ref-man-web-extract-import-link-data (buf)
  "Before call should check the buffer as it can't be called if
buffer is not gscholar"
  (interactive)
  (save-excursion
    (let* ((link (ref-man-eww--gscholar-get-previous-non-google-link buf))
           (args (ref-man-web-get-import-link-data buf link)))
      ;; :link link :link link-text :metadata metadata
      (-concat (list :link link) args))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END eww utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START eww callable functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE: This is the big function
;; If eww is called from any other place, set the
;; ref-man--org-gscholar-launch-buffer and
;; ref-man--org-gscholar-launch-point to nil
(defun ref-man-web-gscholar (url)
  "Fetch URL and render the page.
If the input doesn't look like a URL or a domain name."
  (interactive
   (let* ((uris (eww-suggested-uris))
	  (prompt (concat "Enter URL or keywords"
			  (if uris (format " (default %s)" (car uris)) "")
			  ": ")))
     (list (read-string prompt nil nil uris))))
  (if (eq major-mode 'org-mode)
      (progn (setq ref-man--org-gscholar-launch-buffer (current-buffer))
             (setq ref-man--org-gscholar-launch-point (point)))
    (setq ref-man--org-gscholar-launch-buffer nil))
  (setq url (string-trim url))
  (cond ((string-match-p "\\`file:/" url))
	;; Don't mangle file: URLs at all.
        ((string-match-p "\\`ftp://" url)
         (user-error "FTP is not supported"))
        (t
	 ;; Anything that starts with something that vaguely looks
	 ;; like a protocol designator is interpreted as a full URL.
         (if (or (string-match "\\`[A-Za-z]+:" url)
		 ;; Also try to match "naked" URLs like
		 ;; en.wikipedia.org/wiki/Free software
		 (string-match "\\`[A-Za-z_]+\\.[A-Za-z._]+/" url)
		 (and (= (length (split-string url)) 1)
		      (or (and (not (string-match-p "\\`[\"'].*[\"']\\'" url))
			       (> (length (split-string url "[.:]")) 1))
			  (string-match eww-local-regex url))))
             (progn
               (unless (string-match-p "\\`[a-zA-Z][-a-zA-Z0-9+.]*://" url)
                 (setq url (concat "http://" url)))
               ;; Some sites do not redirect final /
               (when (string= (url-filename (url-generic-parse-url url)) "")
                 (setq url (concat url "/"))))
           (progn ; (setq query-string url)
                  (setq url (concat "https://scholar.google.com/scholar?q="
                                    (replace-regexp-in-string " " "+" url)))))))
  ;; FIXME: Fix this code!
  ;; CHECK: Did I write the chrome code for nothing?
  ;; CHECK: I think this is broken [2020-01-12 Sun 20:40]
  ;; NOTE: Commented out below section [2020-01-12 Sun 21:01]
  ;; (string-match-p "scholar\\.google\\.com" url)
  ;; (let ((buf (generate-new-buffer " *scholar*")))
  ;;   (with-current-buffer buf (insert (gscholar-bibtex-google-scholar-search-results
  ;;                                     query-string)))
  ;;   (if (get-buffer "*google-scholar*")
  ;;       (kill-buffer (get-buffer "*google-scholar*")))
  ;;   (when (get-buffer "*html*")
  ;;     (with-current-buffer (get-buffer "*html*")
  ;;       (setq-local buffer-read-only nil)))
  ;;   (shr-render-buffer buf)
  ;;   (pop-to-buffer-same-window "*html*")
  ;;   (rename-buffer "*google-scholar*")
  ;;   (kill-buffer buf))
  (progn
    (pop-to-buffer-same-window
     (if (eq major-mode 'eww-mode)
         (current-buffer)
       (get-buffer-create "*eww*")))
    (eww-setup-buffer)
    (plist-put eww-data :url url)
    (plist-put eww-data :title "")
    (eww-update-header-line-format)
    (let ((inhibit-read-only t))
      (insert (format "Loading %s..." url))
      (goto-char (point-min)))
    (url-retrieve url 'eww-render
                  (list url nil (current-buffer)))))

;; Much cleaner code now
;; Although, I think I'll remove the debug code later
;; FIXME: The point remains at the end when the function is called again
;; TODO: if the cursor is on a link, I get two pdf links
;; TODO: ref-man-eww-keypress-d and maybe some others don't work
;;       as expected. The keypresses on regions of google-scholar
;;       which are not links, give me incorrect results.
;; NOTE: They're used in the loop. Should use let though, I'll test with let later
(defvar ref-man--egal--save-point)
(defvar ref-man--eww-buffer-links)
(defvar ref-man--egal--prev-url)
(defvar ref-man--egal--current-url)
(defvar ref-man--egal--url-text-start)
(defvar ref-man--egal--url-text-end)
(defvar ref-man--eww-buffer-endpoint)
(make-obsolete-variable 'ref-man--egal--save-point nil "ref-man 0.3.0")
(make-obsolete-variable 'ref-man--eww-buffer-links nil "ref-man 0.3.0")
(make-obsolete-variable 'ref-man--egal--prev-url nil "ref-man 0.3.0")
(make-obsolete-variable 'ref-man--egal--current-url nil "ref-man 0.3.0")
(make-obsolete-variable 'ref-man--egal--url-text-start nil "ref-man 0.3.0")
(make-obsolete-variable 'ref-man--egal--url-text-end nil "ref-man 0.3.0")
(make-obsolete-variable 'ref-man--eww-buffer-endpoint nil "ref-man 0.3.0")
(defun ref-man-web-get-all-links (&optional buf frombegin before-point substring)
  "Get all links from an `shr' buffer BUF.
If BUF is nil it defaults to *eww*.  A non-nil FROMBEGIN gets all
the links in the buffer.  If BEFORE-POINT is non-nil, restrict
beginning of the search to current point otherwise till `eob'.
Use optional regexp SUBSTRING to filter the urls."
  (interactive)
  (save-excursion
    (let ((buf (if buf buf (get-buffer "*eww*"))))
      (with-current-buffer buf            ; should be an shr buffer; usually *eww*
        (setq ref-man--egal--save-point (point))
        (if before-point (setq ref-man--eww-buffer-endpoint (point))
          (setq ref-man--eww-buffer-endpoint (buffer-end 1)))
        (if frombegin (goto-char (point-min)))
        (setq ref-man--eww-buffer-links nil)
        ;; Below was ref-man--egal--current-url
        ;; WTF is egal? eww-get-all-links?
        (setq ref-man--egal--prev-url (get-text-property (point) 'shr-url))
        (setq ref-man--egal--current-url nil)
        (setq ref-man--egal--url-text-start (point))
        (setq ref-man--egal--url-text-end (point))
        (while (< (point) ref-man--eww-buffer-endpoint)
          ;; (debug)
          ;; Debug info
          ;; (message (concat (format "%s" ref-man--egal--url-text-start) ", "
          ;; (format "%s" ref-man--egal--url-text-end)))
          ;; (message (format "%s" (string-match-p substring
          ;; (buffer-substring-no-properties ref-man--egal--url-text-start ref-man--egal--url-text-end))))
          (when (and ref-man--egal--prev-url
                     (stringp ref-man--egal--prev-url))
            (if substring
                (when (string-match-p substring ref-man--egal--prev-url)
                  (setq ref-man--eww-buffer-links
                        (nconc ref-man--eww-buffer-links (list ref-man--egal--prev-url))))
              (setq ref-man--eww-buffer-links
                    (nconc ref-man--eww-buffer-links (list ref-man--egal--prev-url)))))
          (setq ref-man--egal--prev-url ref-man--egal--current-url)
          (setq ref-man--egal--current-url nil)
          (setq ref-man--egal--url-text-start (+ 1 (point)))
          (setq ref-man--egal--url-text-end (+ 1 (point)))
          (while (and (not (eobp))
                      (equal (get-text-property (point) 'shr-url) ref-man--egal--prev-url))
            (forward-char 1))               ;; not next link (same link)
          (setq ref-man--egal--url-text-end (point))
          (setq ref-man--egal--current-url (get-text-property (point) 'shr-url)))
        (goto-char ref-man--egal--save-point))))
  ref-man--eww-buffer-links)

;; ONLY Called from ref-man-eww-keypress-b
(defun ref-man-eww-get-bibtex-from-scholar (&optional to-org)
  "Extract bibtex from Google Scholar.
Extracts the next bibtex entry, or if a url is at point, the
current url from a Google Scholar page.  Optional TO-ORG has to
be an org buffer.  When given, store the extracted bibtex entry
to that buffer."
  (interactive)
  (save-excursion
    (let ((bib-url (progn (search-forward "import into bibtex")
                          (backward-char)
                          (car (eww-links-at-point)))))
          ;; (to-org (if (eq current-prefix-arg 4) nil t)))
      (ref-man-eww--browse-url bib-url nil to-org))))

;; TODO: Make this async with org-property insertion
;;       (ref-man-eww-download-pdf url) already has it, perhaps due to async callback
(defun ref-man-eww-view-and-download-if-required-pdf (url)
  "View the pdf for URL.
If the file corresponding to URL exists in the download directory
then view else download it.  Calls
`ref-man-maybe-create-or-insert-org-heading-and-property' to
delegate org link handling.  If already at URL, url is non nil
and it's checked if it's downloadable, else fetch a pdf for the
URL above it."
  (interactive)
  (if (eq major-mode 'eww-mode)
      (let* ((buf (current-buffer))         ; whatever its name is, but an eww buffer
             (url (if (and url (ref-man-url-downloadable-pdf-url-p url)) ; if it can be downloaded
                      url (car (last (ref-man-web-get-all-links buf t t "pdf"))))) ; else
             (file (ref-man-files-check-pdf-file-exists url t)))
        ;; (ref-man-maybe-create-or-insert-org-heading-and-property file)
        (if file
            (find-file-other-window file)
          (ref-man-eww-download-pdf url t)))
    (message "[ref-man] Not in eww-mode")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END eww callable functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ref-man-web)
