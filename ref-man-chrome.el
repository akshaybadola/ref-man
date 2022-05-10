;;; ref-man-chrome.el --- Module to route the eww requests through chromium so that google doesn't complain about lack of javascript. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019,2020,2021,2022
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Tuesday 10 May 2022 09:07:17 AM IST>
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
;; Extension to `ref-man-web' primarily for accessing google scholar.  In case
;; Google starts to complain about enabling javascript we can proxy the requests
;; through a headless chromium/chrome.
;;
;; The need for this arose as I had written custom `eww' functions for searching
;; extracting pdfs and bibliographic data from google scholar.  I don't think it
;; violates any google policy as I'm interacting with the page myself and not
;; via an automated script.  Although a good configurable browser can be used
;; instead of `eww', `eww' loads pages faster and it's easier to write functions
;; directly in Lisp for interaction with Emacs.

;;; Code:

(require 'a)
(require 'cl-lib)
(require 'eww)
(require 'org)
(require 'dash)
(require 'subr-x)
(require 'url-http)
(require 'websocket)

(require 'ref-man-web)

;; NOTE: Even though chromium supports different user directories, latest
;;       editions of chromium do not let you sign into google services if you
;;       use a non default user-data-dir. I'm not sure why this is
;;       happening. That leads to increased captcha queries from google which is
;;       really annoying.
(defcustom ref-man-chrome-user-data-dir
  (concat (getenv "HOME") "/.config/chromium")
  "Chromium profile directory."
  :type 'directory
  :group 'ref-man)

(defcustom ref-man-chrome-command ""
  "The chromium executable to use.
If this is non-nil then `ref-man-chrome--which-chromium' always
returns its value instead of searching for the chromium
executable."
  :type 'string
  :group 'ref-man)

(defcustom ref-man-chrome-use-proxy nil
  "Whether to use proxy for chromium requests also.
If non-nil the chromium process is started with an option
\"--proxy-server=http://localhost:`ref-man-proxy-port'\"."
  :type 'boolean
  :group 'ref-man)

(defvar ref-man-chrome-chromium-names
  '("chromium-browser" "chromium" "chromium-freeworld" "google-chrome")
  "Possible executable names to check for Chromium Browser.
The names are checked in order for the current active PATH variable.")

(defcustom ref-man-chrome-port-number-start-from
  9222
  "Where to start port numbers for the debugger."
  :type 'interger
  :group 'ref-man)

(defcustom ref-man-chrome-history-limit
  50
  "How many pages to save in memory.  Basically the page source is saved."
  :type 'interger
  :group 'ref-man)

(defvar ref-man-chrome-verbose
  nil
  "If non-nil then much more verbose information is printed.
Mostly for debugging purposes.")

(defvar ref-man-chrome-loud
  nil
  "If non-nil then MUCH MUCH more verbose information is printed.
For debugging purposes.")

(defvar ref-man-chrome-history
  nil
  "List containing the `ref-man-chrome-mode' histories.")

(defvar ref-man-chrome-history-data
  nil
  "Plist containing the `ref-man-chrome-mode' history data.")

(defvar ref-man-chrome-mode-hook
  nil
  "Hook which is run after entering `ref-man-chrome-mode'.")

(defvar ref-man-chrome-history-position
  0
  "Current position in the history.")

;; NOTE: Variables local to file but still in dynamic scope
(defvar ref-man-chrome-data
  nil
  "List which holds all the properties of *chrome* buffer.")
(defvar ref-man-chrome--chromium-port
  nil
  "This variable is set from outside the file.")
(defvar ref-man-chrome--history-list
  nil
  "List holding URLs and SOURCE of visited pages for easier reloading.")
(defvar ref-man-chrome--rpc-id
  0
  "Initial value of rpc-id.")
(defvar ref-man-chrome--rpc-callbacks
  nil
  "Initial value of rpc-callbacks.")
(defvar ref-man-chrome-socket-alist
  nil
  "List containing open chromium debugging sockets.")
(defvar ref-man-chrome-tab-plist
  nil
  "Alist which maps tabs to debugging sockets.")
(defvar ref-man-chrome--initialized-p
  nil
  "Whether the chromium module is initialized or not.
It's set to t after the *chromium* process starts and debugger
sockets are opened and verified.")
(defvar ref-man-chrome--fetch-no-pop
  nil
  "Variable controlling whether to pop to *chrome* buffer.")
(defvar ref-man-chrome--page-url
  nil
  "Variable containing value of current *chrome* URL.")
(defvar ref-man-chrome--page-title
  nil
  "Variable containing value of current *chrome* TITLE.")
(defvar ref-man-chrome--page-source
  nil
  "Variable containing value of current *chrome* SOURCE.")
(defvar ref-man-chrome--nr-page-source
  nil
  "Variable containing value of current no-render SOURCE.")
(defvar ref-man-chrome--nr-page-title
  nil
  "Variable containing value of current no-render TITLE.")
(defvar ref-man-chrome--nr-page-url
  nil
  "Variable containing value of current no-render URL.")
;; FIXME: This variable should be better managed
(defvar ref-man-chrome--check-str "")
(defvar ref-man-chrome--history-table
  nil
  "Table containing *chrome* histories.")

;; NOTE: Internal variables. These are set inside functions and used as sort of
;;       message passing
;;
;; CHECK: If a closure would be enough for these
(defvar ref-man-chrome--tab-id)

;; NOTE: External variables
;; from package `url'
(defvar url-http-end-of-headers)

;; from `ref-man'
(defvar ref-man-proxy-port)
(defvar ref-man-pdf-proxy-port)

;; from `ref-man-core'
(defvar ref-man--org-gscholar-launch-buffer)
(defvar ref-man--org-gscholar-launch-point)

;; NOTE: External functions
(declare-function ref-man--check-bibtex-string "ref-man-core")
(declare-function ref-man-parse-bibtex "ref-man-core")
(declare-function ref-man-org-import-link "ref-man-core")
(declare-function ref-man-import-pdf-url-to-org-buffer "ref-man-core")
(declare-function ref-man-eww-download-pdf "ref-man-core")
(declare-function ref-man-web-get-previous-pdf-link "ref-man-web")
(declare-function ref-man-eww--gscholar-get-previous-non-google-link "ref-man-core")
(declare-function ref-man-get-title-according-to-mode "ref-man-core")
(declare-function find-open-port "ref-man-core")


;; NOTE: Used only on scholar.google.com
;;
;; FIXME: Should be fixed for browsing other URLs, though in that case I can
;;        just go with *eww*
(defconst ref-man-chrome-root-url
  "https://scholar.google.com/"
  "Google Scholar URL from which relative URLs will be computed.")

;; FIXME: The key bindings for ref-man-chrome and ref-man-eww are inconsistent
;;
;; TODO: Actually ref-man-chrome should be renamed ref-man-eww and we should go
;;       to gscholar only if google becomes annoying. It should happen
;;       automatically with ref-man-chrome being a minor mode to ref-man-eww
(defvar ref-man-chrome-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'ref-man-chrome-reload)
    (define-key map "G" 'ref-man-chrome-browse-url)
    ;; (define-key map [?\M-\r] 'eww-open-in-new-buffer)
    (define-key map [?\t] 'shr-next-link)
    (define-key map [?\M-\t] 'shr-previous-link)
    (define-key map [backtab] 'shr-previous-link)
    (define-key map "\r" 'ref-man-chrome-browse-url-at-point)
    (define-key map [delete] 'scroll-down-command)
    (define-key map "q" 'bury-buffer)
    (define-key map "l" 'ref-man-chrome-back-url)
    (define-key map "r" 'ref-man-chrome-forward-url)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    ;; (define-key map "u" 'eww-up-url)
    ;; (define-key map "t" 'eww-top-url)
    (define-key map "&" 'ref-man-chrome-browse-with-external-browser)
    (define-key map "d" 'ref-man-chrome-download-pdf)
    (define-key map "w" 'ref-man-chrome-copy-url)
    ;; (define-key map "C" 'url-cookie-list)
    (define-key map "u" 'ref-man-chrome-view-source)
    ;; (define-key map "R" 'eww-readable)
    (define-key map "H" 'ref-man-chrome-list-histories)
    ;; (define-key map "E" 'eww-set-character-encoding)
    ;; (define-key map "s" 'eww-switch-to-buffer)
    (define-key map "S" 'ref-man-chrome-list-buffers)
    ;; (define-key map "F" 'eww-toggle-fonts)
    ;; (define-key map "D" 'eww-toggle-paragraph-direction)
    ;; (define-key map [(meta C)] 'eww-toggle-colors)
    (define-key map "]" 'ref-man-chrome-gscholar-next)
    (define-key map "[" 'ref-man-chrome-gscholar-previous)
    (define-key map "Q" 'ref-man-chrome-current-gscholar-query-with-range)
    ;; TODO: Bookmarks should be persistent
    (define-key map "b" 'ref-man-chrome-add-bookmark)
    ;; TODO: Has to be a separate bookmarks mode
    (define-key map "B" 'ref-man-chrome-list-bookmarks)
    (define-key map "i" nil)
    (define-key map "ii" 'ref-man-chrome-insert-link-to-org)
    (define-key map "ie" 'ref-man-chrome-insert-everything-to-org)
    (define-key map "e" nil) ; extract things
    (define-key map "eb" 'ref-man-chrome-extract-bibtex) ; extract bibtex
    ;; (define-key map "ep" 'ref-man-chrome-extract-bibtex) ; extract pdf?
    (define-key map "v" 'ref-man-chrome-view/download-pdf)
    ;; TODO Should be only in bookmarks mode
    ;; (define-key map [(meta n)] 'eww-next-bookmark)
    ;; (define-key map [(meta p)] 'eww-previous-bookmark)
    map))

(defvar ref-man-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'shr-show-alt-text)
    (define-key map "i" nil)
    (define-key map "ii" 'ref-man-chrome-insert-link-to-org)
    (define-key map "ie" 'ref-man-chrome-insert-everything-to-org)
    (define-key map "z" 'shr-zoom-image)
    (define-key map [?\t] 'shr-next-link)
    (define-key map [?\M-\t] 'shr-previous-link)
    (define-key map "I" 'shr-insert-image)
    (define-key map "w" 'ref-man-chrome-copy-url)
    (define-key map "u" 'ref-man-chrome-view-source)
    (define-key map "RET" 'ref-man-chrome-browse-url)
    (define-key map "o" 'nil)
    (define-key map "\r" 'ref-man-chrome-browse-url)
    map))

;; CHECK: Maybe save history also
(define-derived-mode ref-man-chrome-mode fundamental-mode "ref-man-chrome"
  "Mode for browsing websites with Emacs bypassing their JS.

Used when the websites want to spy on you and you don't care but
you want Emacs integration with `org-mode', so you run a chrome
headless instance with some google account signed in so that the
website doesn't call you a \"robot\" or doesn't block you for not
using JS or whatever.  WTF, right?

\\{ref-man-chrome-mode-map}"
  (setq-local ref-man-chrome-data (list :url ref-man-chrome--page-url))
  (ref-man-chrome-setup-buffer)
  ;; (setq-local browse-url-browser-function #'eww-browse-url)
  ;; (add-hook 'after-change-functions #'eww-process-text-input nil t)
  ;; desktop support
  ;; CHECK: Do I need this?
  ;; multi-page isearch support
  ;; (setq-local multi-isearch-next-buffer-function #'eww-isearch-next-buffer)
  (setq truncate-lines t)
  (buffer-disable-undo)
  (setq buffer-read-only t))

;; START ref-man-chrome commands

;; (defun ref-man-chrome--get-all-links)
(defun ref-man-chrome-extract-bibtex-from-scholar (&optional org-buf bib-buf org-point)
  "Like `ref-man-eww-get-bibtex-from-scholar'.

Earlier I had thought that it would extract more data or from
more websites, as of now will only extract bibtex and insert to
org.  Optionally extract to org buffer ORG-BUF at optional point
ORG-POINT or bib buffer BIB-BUF."
  (interactive)
  (save-excursion
    (let ((bib-url (progn (search-forward "import into bibtex")
                          (backward-char)
                          (car (eww-links-at-point))))
          (url (ref-man-eww--gscholar-get-previous-non-google-link (current-buffer))))
      (setq ref-man-chrome--check-str "title=")
      (let* ((src (plist-get (ref-man-chrome--set-location-fetch-html bib-url 1 t t) :source))
             (buf (get-buffer-create "*rfc-nr-source*"))
             (dom (with-current-buffer buf
                    (erase-buffer)
                    (insert src)
                    (libxml-parse-html-region (point-min) (point-max))))
             (source-buf (get-buffer-create "*rfc-bib-buf*")))
        (when (ref-man--check-bibtex-string
               (with-current-buffer source-buf
                 (erase-buffer)
                 (shr-insert-document dom)
                 (buffer-string)))
          (ref-man-parse-bibtex source-buf org-buf org-point bib-buf url)
          ;; (cond (bib-buf )
          ;;       (org-buf (ref-man-parse-bibtex source-buf org-buf))
          ;;       (t (ref-man-parse-bibtex source-buf nil nil t)))
          )))))

;; (defun ref-man-chrome-extract-bibtex/old (&optional org-buf)
;;   (interactive (list (when (and current-prefix-arg (not (boundp 'org-buf)))
;;                        (ido-completing-read "Org buffer: "
;;                                             (mapcar
;;                                              (lambda (x) (format "%s" x))
;;                                              (-filter
;;                                               (lambda (x) (with-current-buffer x
;;                                                             (eq major-mode 'org-mode)))
;;                                               (buffer-list)))))))
;;   (when (string-match-p "scholar\\.google\\.com" ref-man-chrome--page-url)
;;     (ref-man-chrome-extract-bibtex-from-scholar org-buf)))

;; TODO: This should extract bibtex based on scholar/arxiv etc, currently only
;;       does from gscholar, but actually, I only need the chrome function to
;;       work on gscholar, as the rest can be handled with eww directly
;; TODO: Maybe extract to a file
(defun ref-man-chrome-extract-bibtex ()
  "Extract bibtex from *chrome* buffer.
With a single `C-u' prefix import to user specified org
buffer.  With a double `C-u' `C-u' prefix import to user specified
bibtex buffer.

Currently only works with google-scholar.  When extracting to an
org buffer, also extracts url as previous non-google url from
gscholar buffer."
  (interactive)
  (if (eq major-mode 'ref-man-chrome-mode)
      (let ((args (pcase
                      (pcase current-prefix-arg
                        ('(4) (list "Org" 'org-mode))
                        ('(16) (list "Bib" 'bibtex-mode)))
                    (`(,str ,mode)
                     (list mode (ido-completing-read
                                 (concat str " buffer: ")
                                 (mapcar (lambda (x)
                                           (format "%s" x))
                                         (-filter (lambda (x)
                                                    (with-current-buffer x
                                                      (eq major-mode mode)))
                                                  (buffer-list))))))
                    (_ '(nil nil)))))
        (pcase args
          (`(,mode ,buf)
           (cond ((eq mode 'org-mode)
                  ;; This one is a bit slow, would prefer to extract bibtex
                  ;; async from the URL
                  (ref-man-chrome-import-everything (current-buffer) buf t))
                 ((eq mode 'bibtex-mode)
                  (ref-man-chrome-extract-bibtex-from-scholar nil buf))
                 (t (ref-man-chrome-extract-bibtex-from-scholar))))))
    (message "[ref-man-chrome] Not ref-man-chrome mode")))

(defun ref-man-chrome-gscholar-next ()
  "Goto next google scholar page."
  (interactive)
  (catch 'retval
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "Next")
        (let ((url (get-text-property (- (point) 1) 'shr-url)))
          (when (and url (string-match-p "scholar.*start=" (concat ref-man-chrome--page-url url)))
            (ref-man-chrome-browse-url url)
            (message "[ref-man-chrome] Going to Next Page")
            (throw 'retval t)))))))

(defun ref-man-chrome-gscholar-previous ()
  "Goto previous google scholar page."
  (interactive)
  (catch 'retval
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "Previous")
        (let ((url (get-text-property (- (point) 1) 'shr-url)))
          (when (string-match-p "scholar.*start=" (concat ref-man-chrome--page-url url))
            (ref-man-chrome-browse-url url)
            (message "[ref-man-chrome] Going to Previous Page")
            (throw 'retval t)))))))

(defun ref-man-chrome-insert-link-to-org (&optional update)
  "Extract link at point and insert to an org buffer.

Optionally UPDATE the heading."
  (interactive)
  (ref-man-chrome-insert-to-org
   (or update (if current-prefix-arg 'current 'next)) 'link))

(defun ref-man-chrome-insert-everything-to-org (&optional update)
  "Extract link and metadata at point and insert to an org buffer.

Optionally UPDATE the heading."
  (interactive)
  (ref-man-chrome-insert-to-org
   (or update (if current-prefix-arg 'current 'next)) 'everything))

(defun ref-man-chrome-insert-to-org (update extract)
  "Extract and insert the current entry to an org buffer.
UPDATE can be either 'current or 'next and EXTRACT is one of
'link or 'everything.  When UPDATE is 'next, create a new heading
which is a child of the org heading at point in the org buffer,
else update the heading at point.

The default buffer is `ref-man--org-gscholar-launch-buffer' if
it's non-nil, else `ref-man-org-links-file-path'."
  (cond ((eq extract 'link)
         (ref-man-web-extract-import-link-data (current-buffer)
                                               (-rpartial #'ref-man-org-import-link
                                                          (eq update 'current))))
        ;; TODO: extract everything requires a bunch of async calls
        ((and (eq extract 'everything) (ref-man-web-gscholar-buffer-p))
         (ref-man-chrome-import-everything (current-buffer) nil (eq update 'current)))
        (t (debug))))

(defun ref-man-chrome-import-everything (web-buf &optional org-buf update)
  "Extract link, text and publication properties from *ref-man-web* buffer.
Web buffer WEB-BUF is an `shr' buffer.  Optional ORG-BUF
specifies the org buffer to insert the data.  If UPDATE is
non-nil then update the current entry in the org buffer ORG-BUF
instead of inserting a subheading."
  (let ((func (-rpartial #'ref-man-org-import-link update)))
    (ref-man-web-extract-import-link-data
     web-buf
     (lambda (args)
       (let* ((insert-data (funcall func args))
              ;; FIXME: Why isn't this (or org-buf (plist-get insert-data :buffer))
              (org-buf (plist-get insert-data :buffer))
              ;; CHECK: Why isn't heading used?
              (heading (plist-get insert-data :heading))
              (pt (plist-get insert-data :point)))
         ;; TODO: Should be a generic bibtex function, extract-bibtex-from-neurips
         (ref-man-chrome-extract-bibtex-from-scholar org-buf nil pt) ; extract bibtex to properties
         (ref-man-import-pdf-url-to-org-buffer ; also extract pdf url BUT NOT the pdf file
          (ref-man-web-get-previous-pdf-link web-buf) nil org-buf pt))))))

(defun ref-man-chrome-view-source ()
  "View the source of current *chrome* buffer."
  (interactive)
  (let ((buf (get-buffer-create "*rfc-source*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert ref-man-chrome--page-source)
      (setq-local buffer-read-only t))
    (pop-to-buffer (get-buffer "*rfc-source*"))))

;; TODO: This should not be interactive and should always be called from a
;;       different function, maybe `ref-man-web-search'.
;;       As of now it's used for the [Enter] key in `ref-man-chrome-mode-map'
(defun ref-man-chrome-browse-url (url)
  "Browse a URL with ref-man-chrome.
We have special functions for google scholar."
  (interactive (list (shr-url-at-point current-prefix-arg)))
  (if (not url)
      (call-interactively #'ref-man-chrome-gscholar)
    (setq url (url-encode-url url))
    (unless (string-prefix-p "http" url)
      (setq url (concat ref-man-chrome-root-url (string-remove-prefix "/" url))))
    (ref-man-chrome--set-location-fetch-html url)))

;; NOTE: For conforming naming convention of eww keymap
(defalias 'ref-man-chrome-browse-url-at-point 'ref-man-chrome-browse-url)

(defun ref-man-chrome-reload ()
  "Reload the current *chrome* buffer."
  (interactive)
  (ref-man-chrome--set-location-fetch-html ref-man-chrome--page-url))

;; TODO: There may be a bug in history navigation
(defun ref-man-chrome-back-url ()
  "Go back in `ref-man-chrome' history for the current *chrome* buffer."
  (interactive)
  ;; Most recent value is first
  (if (> (length ref-man-chrome-history) (+ 1 ref-man-chrome-history-position))
      (progn
        (cl-incf ref-man-chrome-history-position)
        (message (format "[ref-man-chrome] Going back to %s"
                         (nth ref-man-chrome-history-position ref-man-chrome-history)))
        (ref-man-update-from-history))
    (message "[ref-man-chrome] Cannot go back any further")))

(defun ref-man-chrome-forward-url ()
  "Go forward in `ref-man-chrome' history for the current *chrome* buffer."
  (interactive)
  (if (> ref-man-chrome-history-position 0)
      (progn
        (cl-decf ref-man-chrome-history-position)
        (message (format "[ref-man-chrome] Going forward to %s"
                         (nth ref-man-chrome-history-position ref-man-chrome-history)))
        (ref-man-update-from-history))
    (message "[ref-man-chrome] Cannot go forward any further")))

(defun ref-man-chrome-copy-url (url)
  "Kill the URL under point in current *chrome* buffer.
If there's no URL under point then the page URL is copied
instead."
  (interactive (list (shr-url-at-point current-prefix-arg)))
  (unless url
    (setq url ref-man-chrome--page-url))
  (when (and (not (string-prefix-p "javascript" url)) (not (string-prefix-p "http" url)))
    (setq url (concat ref-man-chrome-root-url (string-remove-prefix "/" url))))
  (setq url (url-encode-url url))
  (kill-new url)
  (message "[ref-man-chrome] Copied %s" url))

(defun ref-man-chrome-browse-with-external-browser (&optional url)
  "Browse current page URL with external browser.
Like `eww-browse-with-external-browser' and in fact calls that
function."
  (interactive)
  (eww-browse-with-external-browser (or url ref-man-chrome--page-url)))

(defun ref-man-chrome-download-pdf (&optional view)
  "Download the pdf for current google scholar entry.
With optional VIEW, view the pdf also.

If the web buffer was called from an org file and
`ref-man--org-gscholar-launch-buffer' is non-nil then PDF-FILE
property in the property drawer for the org entry at point is
inserted or updated.  See `ref-man--insert-org-pdf-file-property'."
  (interactive)
  (let (
        ;; FIXME: unused
        ;; (url (get-text-property (point) 'shr-url))
        )
    (if (eq major-mode 'ref-man-chrome-mode)
        (ref-man-eww-download-pdf
         (ref-man-web-get-previous-pdf-link (current-buffer)) view)
      (message "[ref-man-chrome] Not ref-man-chrome mode"))))

(defun ref-man-chrome-view/download-pdf ()
  "View or Download PDF file according to buffer state."
  (interactive)
  (ref-man-chrome-download-pdf t))

;; TODO: These are placeholders
(defun ref-man-chrome-list-histories ()
  "List the histories in a *chrome-history* buffer."
  (interactive)
  (message "[ref-man-chrome] Does nothing for now"))

(defun ref-man-chrome-list-buffers ()
  "Not sure actually.
Should list current active *eww* and *chrome* or other `shr' buffers maybe."
  (interactive)
  (message "[ref-man-chrome] Does nothing for now"))

(defun ref-man-chrome-add-bookmark ()
  "Add bookmark."
  (interactive)
  (message "[ref-man-chrome] Does nothing for now"))

(defun ref-man-chrome-list-bookmarks ()
  "List bookmarks in a *bookmarks* buffer."
  (interactive)
  (message "[ref-man-chrome] Does nothing for now"))

;; TODO: Check why code to put text property is not working.
;; NOTE: I think it was interactive for debugging purposes
(defun ref-man-chrome-setup-buffer (&optional dont-save)
  "Setup the *chrome* buffer.
Optional argument DONT-SAVE does not add the URL to the history."
  (interactive)
  (unless dont-save
    (ref-man-chrome-save-history))
  (let ((inhibit-read-only t))
    (remove-overlays)
    ;; (erase-buffer)
    (save-excursion
      (let ((current nil)
            (start nil)
            ;; FIXME: unused
            ;; (end nil)
            )
        (goto-char (point-min))
        (while (not (eobp))
          (let ((prop (get-text-property (point) 'shr-url)))
            (cond ((and prop current (not (equal prop current))) ; transition without pause
                   (put-text-property start (point) 'keymap ref-man-link-map)
                   ;; (when string-prefix-p "/" current
                   ;;       (put-text-property start (point) 'shr-url
                   ;;                          (concat ref-man-chrome-root-url (string-remove-prefix "/" current))))
                   (setq start (point))
                   (setq current (get-text-property (point) 'shr-url)))
                  ((and current (not prop)) ; end
                   ;; (when string-prefix-p "/" current
                   ;;       (put-text-property start (point) 'shr-url
                   ;;                          (concat ref-man-chrome-root-url (string-remove-prefix "/" current))))
                   (setq current nil)
                   (put-text-property start (point) 'keymap ref-man-link-map))
                  ((and prop (not current)) ; begin
                   (setq start (point))
                   (setq current (get-text-property (point) 'shr-url)))))
          (forward-char 1)))))
  ;; (setq bidi-paragraph-direction nil)
  (unless (eq major-mode 'ref-man-chrome-mode)
    (message "[ref-man-chrome] NOT ref-man-chrome-mode in setup-buffer")
    (ref-man-chrome-mode)))

;; FIXME: Clear all history if no buffer active or *chrome* is killed
(defun ref-man-chrome-clear-history ()
  "Clear all history except the current url."
  (interactive)
  (ref-man-chrome-init-history)
  (when ref-man-chrome-history          ; Can't do anything if it's nil
    (cl-loop for x in ref-man-chrome-history
             do (unless (string= ref-man-chrome--page-url x)
                  (remhash x ref-man-chrome--history-table)))
    (setq ref-man-chrome-history `(,ref-man-chrome--page-url))
    (setq ref-man-chrome-history-position 0)))

;; FIXME: these should be moved from here into some config
;; (global-set-key (kbd "C-c e c") 'ref-man-chrome-gscholar)
(defun ref-man-chrome-gscholar (url)
  "Fetch URL with a Chromium process.

Used to bypass annoying \"need JS\" messages from google (or
other sites).  Primarily used for Google Scholar but can be used
for other sites.  Uses a chromium process running in headless
mode with a debugger to fetch pages there and then get html to
eww and render with that.

Mostly derived from `eww' and `eww--dwim-expand-url'."
  (interactive
   (let* ((uris (eww-suggested-uris))
	  (prompt (concat "Enter URL or keywords"
			  (if uris (format " (default %s)" (car uris)) "")
			  ": ")))
     (list (read-string prompt nil nil uris))))
  (unless ref-man-chrome--initialized-p
    (ref-man-chrome-init))
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
                          ;; NOTE: `localhost` was `eww-local-regex'
			  (string-match "localhost" url))))
             (progn
               (unless (string-match-p "\\`[a-zA-Z][-a-zA-Z0-9+.]*://" url)
                 (setq url (concat "http://" url)))
               ;; Some sites do not redirect final /
               (when (string= (url-filename (url-generic-parse-url url)) "")
                 (setq url (concat url "/"))))
           ;; TODO: `ref-man-chrome-root-url' should be set here or somewhere
           (progn ; (setq query-string url)
             (setq url (concat "https://scholar.google.com/scholar?q="
                               (replace-regexp-in-string " " "+" url)))))))
  (unless (string-empty-p url)
    (ref-man-chrome--set-location-fetch-html url)))
(make-obsolete-variable 'ref-man-chrome-gscholar 'ref-man-web-search "ref-man 0.3.2")

(defun ref-man-chrome-current-gscholar-query-with-range (range)
  "Set the year RANGE for current Google Scholar page.

Range is to be entered as a space separate strings and must be
valid 4 digit years.  MIN first and MAX second.  If MAX is omitted
then MIN is the year from which to show results.

E.g., if results from 2010-2018 then \"2010 2018\" should be
entered on the prompt.  If \"2010\" alone is entered then results
from 2010 onwards will be displayed."
  (interactive (list (if (eq major-mode 'ref-man-chrome-mode)
                         (read-from-minibuffer "Enter the range (MIN MAX): ")
                       (message "Not in ref-man-chrome-mode")
                       nil)))
  (when range
    (setq range (split-string range " "))
    (let ((low (car range))
          (hi (nth 1 range))
          (url (string-join (-remove (lambda (x) (or (string-match-p "as_ylo=" x)
                                                     (string-match-p "as_yhi=" x)))
                                     (split-string ref-man-chrome--page-url "&")) "&")))
      (if hi
          (ref-man-chrome-gscholar
           (concat url (format "&as_yhi=%s" hi) (format "&as_ylo=%s" low)))
        (ref-man-chrome-gscholar
         (concat url (format "&as_ylo=%s" low)))))))

;; FIXME: There are two such functions now, and probably one in ref-man-core also
(defun ref-man-chrome-search-on-gscholar ()
  "Search on Google Scholar according to mode.
Uses `ref-man-get-title-according-to-mode' which is used to
extract the query string from compatible modes.

With a `\\[universal-argument]', read query string directly from user
regarding regardless of mode."
  (interactive)
  (unless ref-man-chrome--initialized-p
    (ref-man-chrome-init))
  (let ((query (ref-man-get-title-according-to-mode current-prefix-arg)))
    (if (string-empty-p query)
        (message "[ref-man-chrome] Empty query string")
      (setq query (replace-regexp-in-string ":\\|/" " " query))
      (message (concat "[ref-man-chrome] Fetching " query))
      (ref-man-chrome--set-location-fetch-html
       (concat "https://scholar.google.com/scholar?q="
               (replace-regexp-in-string " " "+" query))))))
(make-obsolete 'ref-man-chrome-search-on-gscholar
               'ref-man-web-search "")

;; (defun ref-man-chrome-search-heading-on-gscholar ()
;;   "Search for the current org heading in google scholar in *chrome*.
;; Stores the buffer and the position from where it was called."
;;   (interactive)
;;   (unless ref-man-chrome--initialized-p
;;     (ref-man-chrome-init))
;;   (if (eq major-mode 'org-mode)
;;       (progn
;;         (setq ref-man--org-gscholar-launch-point (point)) ; CHECK: must be at heading?
;;         (setq ref-man--org-gscholar-launch-buffer (current-buffer))
;;         (save-excursion
;;           (let ((query-string (replace-regexp-in-string ":\\|/" " "
;;                                                         (substring-no-properties (org-get-heading t t)))))
;;             (message (concat "[ref-man-chrome] Fetching " query-string))
;;             (ref-man-chrome--set-location-fetch-html
;;              (concat "https://scholar.google.com/scholar?q="
;;                      (replace-regexp-in-string " " "+" query-string))))))
;;     (message "[ref-man-chrome] Not in org-mode")))
;; (make-obsolete 'ref-man-chrome-search-heading-on-gscholar
;;                'ref-man-chrome-search-on-gscholar "")
;; END ref-man-chrome commands

(defun ref-man-chrome-links-at-point ()
  "Return list of URIs, if any, linked at point."
  (remq nil
	(list (get-text-property (point) 'shr-url)
	      (get-text-property (point) 'image-url))))

(defun ref-man-chrome--which-chromium ()
  "Find the chromium executable.
Returns the first executable found from candidates in
`ref-man-chrome-chromium-names'.  If `ref-man-chrome-command' is
non-nil then the value of that is returned instead."
  (or (not (string-empty-p ref-man-chrome-command))
      (-first #'identity
              (mapcar #'executable-find ref-man-chrome-chromium-names))))

(defun ref-man-chrome--chromium-running ()
  "Check if any chromium process is running.
Only checks for the chromium process associated with
`ref-man-chrome'."
  (or (ref-man-chrome--chromium-process)
      (let* ((chrome (file-name-nondirectory (ref-man-chrome--which-chromium)))
             (grep-string (concat "ps -ef | grep " chrome))
             (text (replace-regexp-in-string ".*grep.*" ""
                                             (shell-command-to-string grep-string))))
        (string-match-p chrome text))))

(defun ref-man-chrome--find-open-port ()
  "Find the next open debugging port for chromium.
We search from `ref-man-chrome-port-number-start-from' in case
`ref-man-chrome-port-number-start-from' is being used by another
process.  Uses `find-open-port'"
  (find-open-port ref-man-chrome-port-number-start-from))

(defun ref-man-chrome--process-helper (headless data-dir port)
  "Helper to start the chromium process.
Optional HEADLESS implies to start in headless mode: no window.
DATA-DIR is where to load/store user data and profiles.  PORT is
which debugging port to open."
  (if ref-man-chrome-use-proxy
      (if headless
          (start-process "chromium" "*chromium*" (ref-man-chrome--which-chromium)
                         (format "--proxy-server=http://localhost:%d" ref-man-proxy-port)
                         "--headless" (concat "--user-data-dir=" data-dir)
                         (format "--remote-debugging-port=%s" port))
        (start-process "chromium" "*chromium*" (ref-man-chrome--which-chromium)
                       (format "--proxy-server=http://localhost:%d" ref-man-proxy-port)
                       (concat "--user-data-dir=" data-dir)
                       (format "--remote-debugging-port=%s" port)))
    (if headless
        (start-process "chromium" "*chromium*" (ref-man-chrome--which-chromium)
                       "--headless" (concat "--user-data-dir=" data-dir)
                       (format "--remote-debugging-port=%s" port))
      (start-process "chromium" "*chromium*" (ref-man-chrome--which-chromium)
                     (concat "--user-data-dir=" data-dir)
                     (format "--remote-debugging-port=%s" port))))
  (message "[ref-man-chrome] Started Chromium process"))

(defun ref-man-chrome--kill-chromium-process ()
  "Kill the chrome process."
  (when (get-process "chromium")
    (signal-process (get-buffer "*chromium*") 15)))

(defun ref-man-chrome--chromium-process ()
  "Get the *chromium* process if running in Emacs."
  (get-process "chromium"))

(defun ref-man-chrome-start-process (&optional headless)
  "Start a new chromium process.
Start in headless mode if HEADLESS is non-nil, else windowed
mode.  When called interactively, start in windowed mode by
default."
  (interactive "p")
  (let ((headless (and headless (pcase headless (1 nil) (_ t))))
        (port (ref-man-chrome--find-open-port))
        (data-dir ref-man-chrome-user-data-dir)
        ;; CHECK: Why isn't this used? I think it was moved to `ref-man-chrome--process-helper'
        (chromium (or (and (processp (ref-man-chrome--chromium-running)) 'internal)
                      (and (integerp (ref-man-chrome--chromium-running)) 'external))))
    (setq ref-man-chrome--chromium-port port)
    (ref-man-chrome--process-helper headless data-dir port)
    ;; NOTE: start process regardless of whether running previously
    ;; (cond ((eq chromium 'internal)
    ;;        (if (y-or-n-p "Existing internal chromium process found.  Kill it and start headless? ")
    ;;            (progn (ref-man-chrome--kill-chromium-process)
    ;;                   (message "[ref-man-chrome] Killing old and starting new chromium.")
    ;;                   (ref-man-chrome--process-helper headless data-dir port))
    ;;          (message "[ref-man-chrome] Not starting chromium")))
    ;;       ((eq chromium 'external)
    ;;        (message "[ref-man-chrome] Chromium running outside emacs. Cannot kill and start new."))
    ;;       ((not chromium)
    ;;        (message "[ref-man-chrome] Starting new chromium.")
    ;;        (ref-man-chrome--process-helper headless data-dir port)))
    ))

(defun ref-man-chrome-restart ()
  "Restart the chrome process.
With a `C-u' universal prefix arugment, reset the history also."
  (interactive)
  (ref-man-chrome-shutdown t current-prefix-arg))

(defun ref-man-chrome--check-port (port)
  "Unused.  I think it was to check if the current PORT is headless chromium."
  (let* ((url (url-parse-make-urlobj
               "http" nil nil "localhost" port "/"))
         (url-request-method "GET")
         (url-http-attempt-keepalives nil)
         (buf (url-retrieve-synchronously url nil nil 1)))
    (with-current-buffer buf
      (when (string-match-p "headless remote" (buffer-string))
        t))))

(defun ref-man-chrome-get-json ()
  "Retrieve the message as JSON from `ref-man-chrome--chromium-port'."
  (let* ((url (url-parse-make-urlobj
               "http" nil nil "localhost" ref-man-chrome--chromium-port "/json"))
         (url-request-method "GET")
         (url-http-attempt-keepalives nil)
         (json-array-type 'list)        ; changes to list instead of vector
         (json-object-type 'plist))     ; Not sure how this effects
    ;; (buf (url-retrieve-synchronously url nil nil 1)))
    (with-current-buffer (url-retrieve-synchronously url)
      (if (not (eq 200 (url-http-parse-response)))
          (error "Unable to connect to host")
        (goto-char (+ 1 url-http-end-of-headers))
        (json-read)))))

(defun ref-man-chrome-get-tabs ()
  "Get all the open tabs from chromium debugger websocket."
  (let* ((json-string (ref-man-chrome-get-json))
         (tabs (-filter (lambda (tab)
                          (and (plist-get tab :webSocketDebuggerUrl)
                               (string-equal (plist-get tab :type) "page")))
                        json-string)))
    tabs))

(defun ref-man-chrome-reset (reset-history)
  "Reset the `ref-man-chrome' plugin.
With non-nil optional RESET-HISTORY, clear the current history
also."
  (setq ref-man-chrome--chromium-port nil)
  (setq ref-man-chrome--rpc-id 0)
  (setq ref-man-chrome--rpc-callbacks nil)
  (setq ref-man-chrome-socket-alist nil)
  (setq ref-man-chrome--initialized-p nil)
  (setq ref-man-chrome-tab-plist nil)
  (when reset-history
    (ref-man-chrome-clear-history)))

(defun ref-man-chrome--encode (data)
  "Encode DATA to json."
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-encode data)))

(defun ref-man-chrome--decode (data)
  "Decode json DATA to elisp structures."
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-read-from-string data)))

(defun ref-man-chrome--next-rpc-id ()
  "Increment the current rpc id.
Incrememnts the internal variable `ref-man-chrome--rpc-id'."
  (setq ref-man-chrome--rpc-id (+ 1 ref-man-chrome--rpc-id))
  ref-man-chrome--rpc-id)

(defun ref-man-chrome--register-callback (id fn)
  "Register callback FN for a given message ID."
  (when ref-man-chrome-verbose
    (message (format "[ref-man-chrome] register-callback, registering callback for %d %s" id fn)))
  (setq ref-man-chrome--rpc-callbacks (plist-put ref-man-chrome--rpc-callbacks id fn)))

(defun ref-man-chrome--dispatch-callback (id data)
  "Dispatch a registered callback for the given message ID.
The callback is called with DATA as argument."
  (let ((hook (plist-get ref-man-chrome--rpc-callbacks id)))
    (if hook
        (progn
          (when ref-man-chrome-verbose
            (message (format "[ref-man-chrome] dispatch-callback, dispatching callback for %s %s" id hook)))
          (cond ((listp hook)
                 (funcall hook data))
                ((or (functionp hook) (symbolp hook))
                 (funcall hook data))
                (t (debug) (message "[ref-man-chrome] dispatch-callback, no callback. Not sure what to do")))
          (setq ref-man-chrome--rpc-callbacks (plist-put ref-man-chrome--rpc-callbacks id nil)))
      (when ref-man-chrome-verbose
        (message "[ref-man-chrome] dispatch-callback, no hook for id %d" id)))))

;; NOTE: It's called method but it's actually THE CALL
(defun ref-man-chrome-call-rpc (method socket &optional params callback)
  "Send the (usually) javascript METHOD over SOCKET.
Optional PARAMS are the debugger params.  Attaches a unique id to
the METHOD and registeres CALLBACK if given for it when the
response comes."
  (let ((id (ref-man-chrome--next-rpc-id)))
    (when ref-man-chrome-verbose
      (if (string= method "Runtime.evaluate")
          (if callback
              (message (concat "[ref-man-chrome] call-rpc, Method: " (plist-get params :expression)
                               " ID: " (format "%s " id) "Added Callback: " (format "%s" callback)))
            (message (concat "[ref-man-chrome] call-rpc, Method: " (plist-get params :expression)
                             " ID: " (format "%s " id) "No Callback")))
        (message (concat "[ref-man-chrome] call-rpc, Method: " method " ID: " (format "%s " id) " No Callback"))))
    (when callback
      (ref-man-chrome--register-callback id callback))
    (websocket-send-text
     socket
     (ref-man-chrome--encode (list :id id
                                   :method method
                                   :params params)))))

(defun ref-man-chrome--on-open (socket)
  "Callback when websocket SOCKET opens."
  (ref-man-chrome-call-rpc "Console.enable" socket)
  (ref-man-chrome-call-rpc "Debugger.enable" socket)
  (ref-man-chrome-call-rpc "Network.setCacheDisabled" socket
                           '(:cacheDisabled t))
  (message (format "[ref-man-chrome]: Opened new websocket %s" socket)))

(defun ref-man-chrome--on-close (socket)
  "Callback when websocket SOCKET closes."
  (message (format "[ref-man-chrome]: closed websocket %s" socket)))


(defun ref-man-chrome--message-added (data)
  "Callback when a message is sent to a websocket.
DATA contains the url, message and other parameters."
  (when data
    (let* ((-message (plist-get data :message))
           ;; NOTE: Not using these
           ;; (url (plist-get message :url))
           ;; (column (plist-get message :column))
           ;; (line (plist-get message :line))
           ;; (type (plist-get message :type))
           ;; (level (plist-get message :level))
           ;; (text (plist-get message :text))
           )
      (when ref-man-chrome-verbose
        (message (format "[ref-man-chrome] on-message-added, Message added: %s" -message))))))

;; (defun kite-mini-on-script-parsed (data)
;;   (let ((extension? (plist-get data :isContentScript))
;;         (url (plist-get data :url))
;;         (id (plist-get data :scriptId)))
;;     (when (and (eq extension? :json-false) (not (string-equal "" url)))
;;       (add-to-list 'kite-mini-rpc-scripts (list :id id :url url)))))

;; (defun kite-mini-on-script-failed-to-parse (data)
;;   (kite-mini-console-append (format "%s" data)))

;; FIXME: What does params even do here and how I can do plist-get without a list?
(defun ref-man-chrome--script-parsed (params &optional id result)
  "Callback when a script sent to chromium is parsed correctly.
PARAMS were the debugger params.  Optional ID was the script ID
and RESULT is the result of parsing from the debugger."
  (if ref-man-chrome-verbose
      (progn
        (if params
            (message (format "[ref-man-chrome] on-script-parsed, Parsed Script\n%s" params))
          (message "[ref-man-chrome] on-script-parsed, Parsed Script. No Params"))
        (if (and id result)
            (progn
              (message (format "[ref-man-chrome] on-script-parsed, BOTH id result %s %s" id result))
              (ref-man-chrome--dispatch-callback (plist-get id params)
                                                 (plist-get result params)))
          (message "[ref-man-chrome] on-script-parsed, NEITHER id result")))
    (when (and id result)
      (message (format "MEOW %s %s" id result))
      (ref-man-chrome--dispatch-callback (plist-get id params)
                                         (plist-get result params)))))

(defun ref-man-chrome--failed-to-parse (params)
  "Callback when a script fails to parse.
PARAMS were the script params."
  (if params
      (message (format "[ref-man-chrome] failed-to-parse, Failed Script\n%s" params))
    (message "[ref-man-chrome] failed-to-parse, Failed Script. No Params")))

;; FIXME: Some weird error being given saying
;;        Error (websocket): in callback `on-message': Wrong type argument: stringp, nil
(defun ref-man-chrome--on-message (socket data)
  "Callback when message is received from websocket SOCKET.
DATA may or may not be present."
  (let* ((data (ref-man-chrome--decode (when data (websocket-frame-payload data))))
         (method (plist-get data :method))
         (params (plist-get data :params))
         (id (plist-get data :id))
         (result (plist-get data :result)))
    (pcase method
      ;; Script is parsed which is to fetch thingy
      ;; ("Debugger.scriptParsed" (ref-man-chrome--script-parsed params id result))
      ("Debugger.scriptParsed" (when ref-man-chrome-verbose
                                 (message (format "on-message %s %s %s" params id result)))
       (ref-man-chrome--script-parsed params id result))
      ;; we are getting an error in Console.messageAdded
      ("Debugger.scriptFailedToParse" (ref-man-chrome--failed-to-parse params))
      ("Console.messageAdded" (ref-man-chrome--message-added params))
      ;; ;; TODO: do something useful here, possibly great for REPL
      ;; ("Console.messageRepeatCountUpdated")
      ;; nil -> These are return messages from RPC calls, not notification
      (_
       (if method
           (when ref-man-chrome-verbose
             (message (format "[ref-man-chrome] on-message, Some method! %s" method)))
         (ref-man-chrome--dispatch-callback id result))))))

(defun ref-man-chrome--open-socket (url)
  "Open websocket to URL."
  (if url
      (message (format "[ref-man-chrome]: Opening socket for %s" url))
      (message "[ref-man-chrome]: Opening socket for no url"))
  (websocket-open url
                  :on-open #'ref-man-chrome--on-open
                  :on-message #'ref-man-chrome--on-message
                  :on-close #'ref-man-chrome--on-close))

;; TODO: These should be macros
(defun ref-man-chrome-get-socket-url-for-tab-indx (indx)
  "Get websocket url for a tab INDX."
  (let ((maybe-url (-filter (lambda (x) (eq (plist-get x :indx) indx)) ref-man-chrome-tab-plist)))
    (unless maybe-url
      (ref-man-chrome-make-tab-plist))
    (plist-get (car (-filter (lambda (x) (eq (plist-get x :indx) indx))
                             ref-man-chrome-tab-plist))
               :socket)))

(defun ref-man-chrome-get-socket-url-for-tab-id (id)
  "Get websocket url for a tab ID."
  (let ((maybe-url (-filter (lambda (x) (equal (plist-get x :id) id)) ref-man-chrome-tab-plist)))
    (unless maybe-url
      (ref-man-chrome-make-tab-plist))
    (plist-get (car (-filter (lambda (x) (equal (plist-get x :id) id))
                             ref-man-chrome-tab-plist))
               :socket)))

(defun ref-man-chrome-make-tab-plist ()
  "Make a plist of chrome tabs associating debugger ids to integers."
  (let* ((tabs (ref-man-chrome-get-tabs))
         (sockets (mapcar (lambda (x)
                            (cons (plist-get x :id) (plist-get x :webSocketDebuggerUrl)))
                          tabs))
         (indx 0))
    ;; Only push ids for new tabs
    (cl-loop for x in sockets
             do (progn (unless (plist-get ref-man-chrome-tab-plist (car x))
                         (push (list :indx indx :id (car x) :socket (cdr x))
                               ref-man-chrome-tab-plist)
                         (cl-incf indx))))))

;; FIXME: If func is called twice, once with ID and once with INDX but for the
;; same tab there'll be duplicate sockets for same tabs.
(defun ref-man-chrome-connect (id-or-indx &optional is-id)
  "Connect to a chromium tab's debugger websocket.
ID-OR-INDX could is internal tab indx to the chromium tab.

Optional IS-ID means consider the ID-OR-INDX as chrome tab id
instead of internal tab indx.

Open the socket if it doesn't exist in
`ref-man-chrome-socket-alist'."
  (let* ((fn (if is-id #'ref-man-chrome-get-socket-url-for-tab-id
               #'ref-man-chrome-get-socket-url-for-tab-indx))
         (socket-url (funcall fn id-or-indx))
         (socket (progn
                   (unless socket-url
                     (error "[ref-man-chrome] No socket url for id-or-indx %s" id-or-indx))
                   (or (a-get ref-man-chrome-socket-alist id-or-indx)
                       (ref-man-chrome--open-socket socket-url)))))
    (unless (websocket-openp socket)
      (setq socket (ref-man-chrome--open-socket socket-url)))
    (unless (eq socket (a-get ref-man-chrome-socket-alist id-or-indx))
      (setq ref-man-chrome-socket-alist
            (a-assoc ref-man-chrome-socket-alist id-or-indx socket)))))

(defun ref-man-chrome-connect-to-empty-tab ()
  "Return a websocket connection for the first empty chrome tab.
Make a new connection if required."
  (ref-man-chrome-connect-to-tab-with-url-re "chrome://new"))

(defun ref-man-chrome-connect-to-tab-with-url-re (url-re)
  "Return a websocket connection for the chrome tab url matching URL-RE.
Make a new connection if required."
  (let* ((tabs (ref-man-chrome-get-tabs))
         (maybe-tab (car (-filter (lambda (x) (string-match-p url-re (plist-get x :url))) tabs)))
         (tab-id (plist-get maybe-tab :id))
         (maybe-socket (a-get ref-man-chrome-socket-alist tab-id)))
    (or maybe-socket
        (and (ref-man-chrome-connect tab-id t)
             (a-get ref-man-chrome-socket-alist tab-id)))))

(defun ref-man-chrome-eval (eval-str id &optional callback)
  "Evaluate (usually javascript) expression EVAL-STR for tab ID (0 or 1).
Optional CALLBACK to call when response is received from
websocket."
  (ref-man-chrome-eval-on-socket
   eval-str
   (a-get ref-man-chrome-socket-alist id)
   callback))

(defun ref-man-chrome-eval-on-socket (eval-str socket &optional callback)
  "Evaluate JS expression EVAL-STR for chrome debugger websocket SOCKET.
Optional CALLBACK to call when response is received from
websocket."
  (unless socket
    (error "[ref-man-chrome] Empty socket given"))
  (unless (websocket-openp socket)
    (error "[ref-man-chrome] Socket is closed"))
  (ref-man-chrome-call-rpc
   "Runtime.evaluate"                   ; method
   socket                               ;socket
   (list :expression eval-str             ; `eval-str' is javascript code usually
         :returnByValue t)              ; params
   callback))

(defun ref-man-chrome-shutdown (&optional restart clear-history)
  "Kill the async process and reset.
Optionally restart the process and reinitialize if RESTART is
non-nil.  With a `C-u' universal prefix arugment or if called
programmatically with the optional CLEAR-HISTORY, reset the
history also."
  (interactive)
  (let ((msg
         (concat "[ref-man-chrome]"
                 (cond ((processp (ref-man-chrome--chromium-running))
                        (if (ref-man-chrome--kill-chromium-process)
                            " Killed chromium process."
                          " Could not kill chromium process."))
                       ((integerp (ref-man-chrome--chromium-running))
                        " Chromium running outside emacs. Cannot kill and start new.")
                       ((not (ref-man-chrome--chromium-running))
                        " No chromium process found.")
                       (t (message "[ref-man-chrome] Something weird happened. Check."))))))
    (setq msg (concat msg " Resetting `ref-man-chrome'."
                      (if (or clear-history current-prefix-arg)
                          " Resetting history also."
                        " Not resetting history.")))
    (ref-man-chrome-reset (or clear-history current-prefix-arg))
    (message msg)
    (when restart
      (sleep-for 1)
      (message " Restarting...")
      (ref-man-chrome-init))))

(defun ref-man-chrome-init-history ()
  "Initialize the history table."
  (setq ref-man-chrome--history-table
        (make-hash-table :test 'equal
                         :size ref-man-chrome-history-limit)))

;; TODO: I don't think shutdown preserves history right now as it always
;;       disables `ref-man-chrome--initialized-p'
;; FIXME: There's some bug here which leads to thousands of buffers
;;        left open.
(defun ref-man-chrome-init (&optional not-headless quiet)
  "Initialze the `ref-man-chrome' plugin.
With a non-nil optional NOT-HEADLESS, the chromium process is
started in windowed mode instead of headless which is default.

Optional non-nil QUIET supresses the messages."
  (interactive)
  (if ref-man-chrome--initialized-p
      (message "[ref-man-chrome] init, Chromium plugin was already initialized.")
    ;; FIXME: Check for shutdown
    (unless ref-man-chrome--history-table
      (ref-man-chrome-init-history))
    (unless (ref-man-chrome--chromium-process)
      (ref-man-chrome-start-process (or current-prefix-arg (not not-headless)))
      ;; FIXME: Should be sleep until socket opens.  Here `sleep-for' waits
      ;;        until the process starts and the chrome debug port
      ;;        opens. However if it's not headless then the port doesn't open
      ;;        for some time. It should instead be an async test and then
      ;;        callback to see if the port has indeed opened.
      (sleep-for .4))
    (let ((ref-man-chrome--init-0 t)
          (ref-man-chrome--init-1 t))
      (ref-man-chrome-connect 0)
      (message "%s" ref-man-chrome-socket-alist)
      (ref-man-chrome-eval "location.href = \"https://scholar.google.com\"" 0
                           (lambda (result)
                             (when ref-man-chrome-verbose
                               (message (concat "[ref-man-chrome] Set URL of tab 0 to "
                                                (plist-get (plist-get result :result) :value))))
                             (setq ref-man-chrome--init-0 nil)))
      (while ref-man-chrome--init-0
        (unless quiet
          (message "[ref-man-chrome] init, WAITING init-0"))
        (sleep-for .2))
      (ref-man-chrome-eval "window.open();" 0
                           (lambda (result)
                             (when ref-man-chrome-verbose
                               (message (concat "[ref-man-chrome] Opened tab 1 "
                                                (plist-get (plist-get result :result) :value))))
                             (setq ref-man-chrome--init-1 nil)))
      (while ref-man-chrome--init-1
        (unless quiet
          (message "[ref-man-chrome] init, WAITING init-1"))
        (sleep-for .2))
      (ref-man-chrome-connect 1))
    (setq ref-man-chrome--initialized-p t)
    (message "[ref-man-chrome] initialized")))

;; CHECK: What's this for?
;; (defun ref-man--bibtex-generate-autokey (names year title)
;;   (let ((autokey (concat bibtex-autokey-prefix-string
;;                          names
;;                          (unless (or (equal names "")
;;                                      (equal year ""))
;;                            bibtex-autokey-name-year-separator)
;;                          year
;;                          (unless (or (and (equal names "")
;;                                           (equal year ""))
;;                                      (equal title ""))
;;                            bibtex-autokey-year-title-separator)
;;                          title)))
;;     (if bibtex-autokey-before-presentation-function
;;         (funcall bibtex-autokey-before-presentation-function autokey)
;;       autokey)))
;; (insert (ref-man--bibtex-generate-autokey "li" "15" "diversity")) ; li15:_diversity

;; FIXME: This is unused
(defun ref-man-chrome--page-load-check (src str)
  "Unused.
Argument SRC page source.
Argument STR string to find in SRC."
  (and str (string-match-p str src)))

;; CHECK: Why am I not waiting for title and stuff here?
(defun ref-man-chrome--fetch-html-no-render (tab-id &optional wait-title)
  "Fetch the html from TAB-ID but don't render to *chrome*.
History or other data variables are not set.  Basically get the
data from the tab.  With non-nil WAIT-TITLE, waits for the title
for the page to appear.  Default is to not wait."
  (setq ref-man-chrome--nr-page-source nil)
  (setq ref-man-chrome--nr-page-title nil)
  (setq ref-man-chrome--nr-page-url nil)
  (when wait-title
      (while (or (not ref-man-chrome--nr-page-title)
                 (string= "" ref-man-chrome--nr-page-title))
        (ref-man-chrome-eval "document.title" tab-id
                             (lambda (result)
                               (setq ref-man-chrome--nr-page-title
                                     (plist-get (plist-get result :result) :value))))
        (sleep-for .2)
        (message "WAITING page-title")))
  (ref-man-chrome-eval "document.location" tab-id
                       (lambda (result)
                         (setq ref-man-chrome--nr-page-url
                               (plist-get (plist-get (plist-get result :result) :value) :href))))
  ;; CHECK: What did this do?
  ;; (when ref-man-chrome--nr-page-url
  ;;   (message "[DEBUG] Yes nr url"))
  ;; (ref-man-chrome-eval "document.documentElement.innerHTML" tab-id
  ;;                      (lambda (result)
  ;;                        (setq ref-man-chrome--nr-page-source
  ;;                              (plist-get (plist-get result :result) :value))))
  (while (or (not ref-man-chrome--nr-page-url)
             (not ref-man-chrome--nr-page-source)
             (and (stringp ref-man-chrome--nr-page-source)
                  (string= "" ref-man-chrome--nr-page-source))
             (not (string-match-p ref-man-chrome--check-str
                                  ref-man-chrome--nr-page-source)))
    (ref-man-chrome-eval "document.documentElement.innerHTML" tab-id
                       (lambda (result)
                         (setq ref-man-chrome--nr-page-source
                               (plist-get (plist-get result :result) :value))))
    (message (concat "[ref-man-chrome] fetch-html-no-render, WAITING fetch-html"))
    ;; sync call
    (sleep-for .2)))

;; FIXME: This should setup all the global ref-man variables
(defun ref-man-chrome--fetch-html (tab-id &optional no-pop no-title)
  "Fetch the html from the specified TAB-ID.
With non-nil optional NO-POP direct to don't pop to the buffer.
NO-TITLE means do not wait for the title to load.  Helps in
fetching data for some pages."
  (if no-pop
      (progn
        ;; NOTE: use `ref-man-chrome--fetch-no-pop' to fetch in background.
        (setq ref-man-chrome--fetch-no-pop t))
    (setq ref-man-chrome--fetch-no-pop nil))
  (setq ref-man-chrome--page-source nil)
  (setq ref-man-chrome--page-title nil)
  (setq ref-man-chrome--page-url nil)
  (unless no-title
    (while (or (not ref-man-chrome--page-title)
               (string= "" ref-man-chrome--page-title))
      (ref-man-chrome-eval "document.title" tab-id
                           (lambda (result)
                             (setq ref-man-chrome--page-title
                                   (plist-get (plist-get result :result) :value))))
      (sleep-for .2)
      (message "[ref-man-chrome] fetch-html, WAITING page-title")))
  (ref-man-chrome-eval "document.location" tab-id
                       (lambda (result)
                         (setq ref-man-chrome--page-url
                               (plist-get (plist-get (plist-get result :result) :value) :href))))
  (while (or (and (not no-title) (not ref-man-chrome--page-title))
             (not ref-man-chrome--page-url)
             (not (and ref-man-chrome--page-source
                       (string-match-p "</body>" ref-man-chrome--page-source))))
    (ref-man-chrome-eval "document.documentElement.innerHTML" tab-id
                       (lambda (result)
                         (setq ref-man-chrome--page-source
                               (plist-get (plist-get result :result) :value))))
    (message "[ref-man-chrome] fetch-html, WAITING page-source")
    (sleep-for .2))                     ; sync call
  ;; NOTE: Just for good measure fetch again and render buffer in callback
  (ref-man-chrome-eval "document.documentElement.innerHTML" tab-id
                       (lambda (result)
                         (setq ref-man-chrome--page-source
                               (plist-get (plist-get result :result) :value))
                         (with-current-buffer (get-buffer-create "*rfc-source*")
                           (when buffer-read-only
                             (setq-local buffer-read-only nil))
                           (erase-buffer)
                           (insert ref-man-chrome--page-source)
                           (ref-man-chrome-render-buffer (get-buffer "*rfc-source*"))
                           (when ref-man-chrome-verbose
                             (message "[ref-man-chrome] HERE"))
                           (with-current-buffer (get-buffer "*chrome*")
                             (setq-local buffer-read-only t)
                             (ref-man-chrome-mode))
                           (when ref-man-chrome-verbose
                             (message "[ref-man-chrome] HERE 2"))
                           (if ref-man-chrome--fetch-no-pop
                               (message "[ref-man-chrome] Not going to pop to CHROME")
                             (message "[ref-man-chrome] WILL pop to CHROME")
                             (pop-to-buffer (get-buffer "*chrome*")))))))

(defun ref-man-update-from-history ()
  "Update the Emacs *chrome* buffer from history if it exists.
Lookup is done on `ref-man-chrome--history-table'."
  (setq ref-man-chrome--page-url (nth ref-man-chrome-history-position ref-man-chrome-history))
  (setq ref-man-chrome-data (gethash ref-man-chrome--page-url ref-man-chrome--history-table))
  (setq ref-man-chrome--page-source (plist-get ref-man-chrome-data :text))
  (setq ref-man-chrome--page-title (plist-get ref-man-chrome-data :title))
  (with-current-buffer (get-buffer-create "*rfc-source*")
    (when buffer-read-only
      (setq-local buffer-read-only nil))
    (erase-buffer)
    (insert ref-man-chrome--page-source)
    (ref-man-chrome-render-buffer (get-buffer "*rfc-source*"))
    (with-current-buffer (get-buffer "*chrome*")
      (setq-local buffer-read-only t)
      (ref-man-chrome-setup-buffer t))))

;; TODO: This should be a generic call in ref-man-html maybe as a convenience function
(defun ref-man-chrome-render-buffer (buffer)
  "Display the HTML rendering of the given BUFFER."
  (interactive (list (current-buffer))) ; interactive for debugging
  (or (fboundp 'libxml-parse-html-region)
      (error "This function requires Emacs to be compiled with libxml2"))
  (with-current-buffer (get-buffer-create "*chrome*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (shr-insert-document
     (with-current-buffer buffer
       (libxml-parse-html-region (point-min) (point-max))))
    (goto-char (point-min))))

;; CHECK: Another way could be to click the link on the chromium page
;;
;; NOTE: There was an issue with the whole process in that fetch page call was
;;       coming too quickly. (sleep-for 1) alleviates that to some extent but
;;       perhaps the wait time can be reduced.
;;       It still gives me an error once in a while. Not sure how to
;;       deal with that.
(defun ref-man-chrome--set-location-fetch-html (url &optional tab-id no-pop no-render)
  "Go to URL in the chrome process and fetch the html.
Optional TAB-ID specifies the chromium tab, default is 0.

With non-nil NO-POP, don't pop to *chrome* buffer.  With non-nill
NO-RENDER, don't render to *chrome* buffer.  Used for direct page
parsing.

The buffer is opened in `ref-man-chrome-mode' and this is the
function responsible for making sure everything is setup
correctly."
  (setq ref-man-chrome--tab-id (or tab-id 0))
  (let ((call-str (format "location.href = \"%s\"" url))
        (ref-man-chrome--fetched-sentinel t))
    (ref-man-chrome-eval call-str ref-man-chrome--tab-id
                         (lambda (result)
                           (when ref-man-chrome-verbose
                             (message (concat "[ref-man-chrome] fetch-html, fetched html "
                                              (plist-get (plist-get result :result) :value))))
                           (setq ref-man-chrome--fetched-sentinel nil)))
    (while ref-man-chrome--fetched-sentinel
      (message (format "[ref-man-chrome] WAITING set-location %s" ref-man-chrome--fetched-sentinel))
      (sleep-for .2))
    (when ref-man-chrome-loud
      (message (format "Result of fetch call %s" (plist-get (plist-get result :result) :value))))
    (if no-render
        (progn
          (message "[ref-man-chrome] Will not render only fetch")
          (ref-man-chrome--fetch-html-no-render ref-man-chrome--tab-id)
          (list :title ref-man-chrome--nr-page-title
                :url ref-man-chrome--nr-page-url
                :source ref-man-chrome--nr-page-source))
      (ref-man-chrome--fetch-html ref-man-chrome--tab-id no-pop))))

;; CHECK: Do we save the no-render pages history also?
(defun ref-man-chrome-save-history ()
  "Save *chrome* history."
  (plist-put ref-man-chrome-data :point (point))
  (plist-put ref-man-chrome-data :text ref-man-chrome--page-source)
  (plist-put ref-man-chrome-data :title ref-man-chrome--page-title)
  (plist-put ref-man-chrome-data :url ref-man-chrome--page-url)
  (puthash ref-man-chrome--page-url (cl-copy-list ref-man-chrome-data)
           ref-man-chrome--history-table)
  ;; update LRU entry
  (setq ref-man-chrome-history (delete ref-man-chrome--page-url ref-man-chrome-history))
  ;; pushed at the front
  (push ref-man-chrome--page-url ref-man-chrome-history)
  ;; NOTE: when using assoc-list
  ;; (push `(,ref-man-chrome--page-url . ,ref-man-chrome-data) ref-man-chrome-history-data)
  (setq ref-man-chrome-data nil)
  (when-let* ((tail (and (> (length ref-man-chrome-history) ref-man-chrome-history-limit)
        	         (nthcdr (- ref-man-chrome-history-limit 1) ref-man-chrome-history))))
    ;; NOTE: when using assoc-list
    ;; (cl-loop for x in (cdr tail)
    ;;       do (setq ref-man-chrome-history-data
    ;;                (delete (assoc-string x ref-man-chrome-history-data)
    ;;                        ref-man-chrome-history-data)))
    ;; NOTE: This will delete x but not required since VALUE is replaced with puthash earlier
    ;; (cl-loop for x in (cdr tail)
    ;;       do (remhash x ref-man-chrome--history-table))
    (setcdr tail nil)))

;; TODO: Functions to implement
;; (defun ref-man-chrome-close-tab (which-tab) )

;; TODO: Better maps and explanations
;; ;; This might be a bit complicated as the buffer will be rendered as eww-buffer
;; ;; anyway. Only the html parts of the page will be rendered and the rest will be
;; ;; left out. As such it becomes mostly a static page. Only the interactions
;; ;; which change the links on the page need to be mapped back to eww
;; ;;
;; ;; For now I'll just map the ones which pertain to my custom google scholar commands
;; (defvar ref-man-chrome-mode-map         ; eww mode map compatibility
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "g" 'eww-reload) ;FIXME: revert-buffer-function instead!
;;     (define-key map "G" 'eww)
;;     (define-key map [?\M-\r] 'eww-open-in-new-buffer)
;;     (define-key map [?\t] 'shr-next-link)
;;     (define-key map [?\M-\t] 'shr-previous-link)
;;     (define-key map [backtab] 'shr-previous-link)
;;     (define-key map [delete] 'scroll-down-command)
;;     (define-key map "l" 'eww-back-url)
;;     (define-key map "r" 'eww-forward-url)
;;     (define-key map "n" 'eww-next-url)
;;     (define-key map "p" 'eww-previous-url)
;;     (define-key map "u" 'eww-up-url)
;;     (define-key map "t" 'eww-top-url)
;;     (define-key map "&" 'eww-browse-with-external-browser)
;;     (define-key map "d" 'eww-download)
;;     (define-key map "w" 'eww-copy-page-url)
;;     (define-key map "C" 'url-cookie-list)
;;     (define-key map "v" 'eww-view-source)
;;     (define-key map "R" 'eww-readable)
;;     (define-key map "H" 'eww-list-histories)
;;     (define-key map "E" 'eww-set-character-encoding)
;;     (define-key map "s" 'eww-switch-to-buffer)
;;     (define-key map "S" 'eww-list-buffers)
;;     (define-key map "F" 'eww-toggle-fonts)
;;     (define-key map "D" 'eww-toggle-paragraph-direction)
;;     (define-key map [(meta C)] 'eww-toggle-colors)
;;     (define-key map "b" 'eww-add-bookmark)
;;     (define-key map "B" 'eww-list-bookmarks)
;;     (define-key map [(meta n)] 'eww-next-bookmark)
;;     (define-key map [(meta p)] 'eww-previous-bookmark)

;;     (easy-menu-define nil map ""
;;       '("Eww"
;; 	["Exit" quit-window t]
;; 	["Close browser" quit-window t]
;; 	["Reload" eww-reload t]
;; 	["Follow URL in new buffer" eww-open-in-new-buffer]
;; 	["Back to previous page" eww-back-url
;; 	 :active (not (zerop (length eww-history)))]
;; 	["Forward to next page" eww-forward-url
;; 	 :active (not (zerop eww-history-position))]
;; 	["Browse with external browser" eww-browse-with-external-browser t]
;; 	["Download" eww-download t]
;; 	["View page source" eww-view-source]
;; 	["Copy page URL" eww-copy-page-url t]
;; 	["List histories" eww-list-histories t]
;; 	["Switch to buffer" eww-switch-to-buffer t]
;; 	["List buffers" eww-list-buffers t]
;; 	["Add bookmark" eww-add-bookmark t]
;; 	["List bookmarks" eww-list-bookmarks t]
;; 	["List cookies" url-cookie-list t]
;; 	["Toggle fonts" eww-toggle-fonts t]
;; 	["Toggle colors" eww-toggle-colors t]
;;         ["Character Encoding" eww-set-character-encoding]
;;         ["Toggle Paragraph Direction" eww-toggle-paragraph-direction]))
;;     map))


(defvar ref-man-chrome-parse-linkedin-server-str "from flask import Flask, request
from bs4 import BeautifulSoup


app = Flask(__name__)
write_file = 'linkedin_applicants'


@app.route('/write', methods=['POST'])
def parse_and_write_to_file():
    data = request.json.get('page')
    soup = BeautifulSoup(data, features='lxml')
    vcards = soup.find_all('div', {'class': 'vcard'})
    write_data = []
    for vcard in vcards:
        name = vcard.find_next('span', {'class': 'given-name'})
        # contains location and industry
        # overview = vcard.find_next('dl', {'class': 'overview-info'})
        # contains [current, past, email, phone, education, keywords]
        info = vcard.find_next('dl', {'class': 'career-info'})
        email = info.find_next('dd', {'class': 'email'})
        phone = info.find_next('dd', {'class': 'phone'})
        # candidate match
        # match = vcard.find_next('div', {'class', 'candidate-match'})
        write_data.append(','.join(map(lambda x: x.text.strip(), [name, email, phone])))
    with open(write_file, 'a') as f:
        f.write('\n' + '\n'.join(write_data))
    return '\n'.join(write_data)


app.run('localhost', 8989)
"
  "The python server script for parsing linkedin data as a string.")

(defun ref-man-chrome-linkedin-num-applicants ()
  "Message the number of applicants from linkedin CSV."
  (interactive)
  (let ((num-lines (string-to-number
                    (car (split-string
                          (shell-command-to-string "wc /home/joe/linkedin_applicants.csv"))))))
    (message (format "done\nTotal applicants: %s\nCurrent Page: %s"
                     num-lines (/ num-lines 25)))))

(defun ref-man-chrome-http-buffer-data (buf)
  "Get html from an http buffer BUF."
  (with-current-buffer buf
    (goto-char (point-min))
    (re-search-forward "\r?\n\r?\n")
    (message (string-trim (buffer-substring-no-properties (point) (point-max))))))

(defun ref-man-chrome-message-http-buffer (buf)
  "Get html from a chrome http buffer BUF."
  (message (string-trim (ref-man-chrome-http-buffer-data buf))))

(provide 'ref-man-chrome)

;;; ref-man-chrome.el ends here
