;; ref-man-chrome.el --- Module to route the eww requests through chromium so that google doesn't complain about lack of javascript. Mosty is used to browse google scholar but can be used for any other website. ;;; -*- lexical-binding: t; -*-

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


(require 'websocket)

(defcustom ref-man-chrome-user-data-dir
  (concat (getenv "HOME") "/.config/chromium")
  "Chromium profile directory"
  :type 'directory
  :group 'ref-man)

(defcustom ref-man-chrome-port-number-start-from
  9222
  "Where to start port numbers for the debugger"
  :type 'interger
  :group 'ref-man)

(defcustom ref-man-chrome-history-limit
  50 
  "How many pages to save in memory. Basically the page source is saved."
  :type 'interger
  :group 'ref-man)

(defcustom ref-man-chrome-verbose
  nil
  "If non-nil then much more verbose information is printed"
  :type 'boolean
  :group 'ref-man)

(defvar ref-man-chrome-history
  nil
  "List containing the `ref-man-chrome-mode' histories")

(defvar ref-man-chrome-history-data
  nil
  "Plist containing the `ref-man-chrome-mode' history data")

(defvar ref-man-chrome-mode-hook
  nil
  "Hook which is run after entering `ref-man-chrome-mode'")

(defvar ref-man-chrome-history-position
  0
  "Current position in the history")

(setq ref-man-chrome-data nil)
(setq ref-man-chrome--chromium-port nil)
(setq ref-man-chrome--history-list nil)
(setq ref-man-chrome--rpc-id 0)
(setq ref-man-chrome--rpc-callbacks nil)
(setq ref-man-chrome--sockets nil)
(setq ref-man-chrome--tabs nil)
(setq ref-man-chrome--initialized-p nil)
(setq ref-man-chrome--fetch-no-pop nil)
(setq ref-man-chrome--page-url nil)
(setq ref-man-chrome--page-title nil)
(setq ref-man-chrome--page-source nil)
(setq ref-man-chrome--nr-page-source nil)
(setq ref-man-chrome--nr-page-title nil)
(setq ref-man-chrome--nr-page-url nil)
(setq ref-man-chrome--check-str "")
(unless (boundp 'ref-man-chrome--history-table)
  (setq ref-man-chrome--history-table (make-hash-table :test 'equal
                                                :size ref-man-chrome-history-limit)))

;; NOTE: Used only on scholar.google.com
;; 
;; FIXME: Should be fixed for browsing other URLs, though in that case I can
;;        just go with *eww*
(defconst ref-man-chrome-root-url
  "https://scholar.google.com/"
  "Google Scholar URL from which relative URLs will be computed")

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
    ;; (define-key map "&" 'eww-browse-with-external-browser)
    (define-key map "d" 'ref-man-chrome-download)
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
    ;; TODO: Bookmarks should be persistent
    (define-key map "b" 'ref-man-chrome-add-bookmark)    
    ;; TODO: Has to be a separate bookmarks mode
    (define-key map "B" 'ref-man-chrome-list-bookmarks)
    (define-key map "i" 'ref-man-chrome-insert-to-org)
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
    (define-key map "i" 'ref-man-chrome-insert-to-org)
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
  "Mode for browsing google scholar when google wants to spy on
you and you don't care but you want emacs integration with
org-mode, so you run a chrome headless instance with some google
account signed in so that google doesn't call you a \"robot\".
WTF, right?

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
(defun ref-man-chrome-extract-bibtex-from-scholar (&optional org-buf)
  "Like `ref-man-eww-get-bibtex-from-scholar' but maybe I thought
earlier that it would extract more data or from more websites, as
of now will only extract bibtex and insert to org."
  (interactive)
  (save-excursion
    (let ((bib-url (progn (search-forward "import into bibtex")
                          (backward-char)
                          (car (eww-links-at-point)))))
      (setq ref-man-chrome--check-str "title=")
      (let* ((src (plist-get (ref-man-chrome--set-location-fetch-html bib-url 1 t t) :source))
             (buf (get-buffer-create "*rfc-nr-source*"))
             (dom (with-current-buffer buf
                    (erase-buffer)
                    (insert src)
                    (libxml-parse-html-region (point-min) (point-max))))
             (bib-buf (get-buffer-create "*rfc-bib-buf*")))
        (when (ref-man--check-bibtex-string
               (with-current-buffer bib-buf
                 (erase-buffer)
                 (shr-insert-document dom)
                 (buffer-string)))
          (if org-buf
              (ref-man--parse-bibtex bib-buf org-buf)
            (ref-man--parse-bibtex bib-buf nil nil t)))))))

(defun ref-man-chrome-extract-bibtex (&optional org-buf)
  (interactive (list (when (and current-prefix-arg (not (boundp 'org-buf)))
                       (completing-read "Org buffer: "
                                        (mapcar (lambda (x) (format "%s" x)) (buffer-list))))))
  (when (string-match-p "scholar\\.google\\.com" ref-man-chrome--page-url)
    (ref-man-chrome-extract-bibtex-from-scholar org-buf)))

(defun ref-man-chrome-gscholar-next ()
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

(defun ref-man-chrome-insert-to-org ()
  (interactive)
  ;; NOTE: parse-bib does nothing for now. Was supposed to be used to fetch and
  ;;       insert bib also from gscholar buffer but I think it would be better
  ;;       if that were async
  (let (;; CHECK: What was I trying to do below?
        ;; (parse-bib (string-prefix-p))
        (url ref-man-chrome--page-url))
    (ref-man-import-gscholar-link-to-org-buffer
     (ref-man-eww--gscholar-get-previous-non-google-link
      (current-buffer)))))

(defun ref-man-chrome-view-source ()
  (interactive)
  (let ((buf (get-buffer-create "*rfc-source*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert ref-man-chrome--page-source)
      (setq-local buffer-read-only t))
    (pop-to-buffer (get-buffer "*rfc-source*"))))

(defun ref-man-chrome-browse-url (url)
  "Browse a url with ref-man-chrome. Special functions for google scholar."
  (interactive (list (shr-url-at-point current-prefix-arg)))
  (if (not url)
      (message "[ref-man-chrome] No URL under point")
    (setq url (url-encode-url url))
    (unless (string-prefix-p "http" url)
      (setq url (concat ref-man-chrome-root-url (string-remove-prefix "/" url))))
    (ref-man-chrome--set-location-fetch-html url)))

;; NOTE: For conforming naming convention of eww keymap
(defalias 'ref-man-chrome-browse-url-at-point 'ref-man-chrome-browse-url)

(defun ref-man-chrome-reload ()
  "Browse a url with ref-man-chrome. Special functions for google scholar."
  (interactive)
  (ref-man-chrome--set-location-fetch-html ref-man-chrome--page-url))

;; TODO: There may be a bug in history navigation
(defun ref-man-chrome-back-url ()
  (interactive)
  ;; Most recent value is first
  (if (> (length ref-man-chrome-history) (+ 1 ref-man-chrome-history-position))
      (progn
        (incf ref-man-chrome-history-position)
        (message (format "[ref-man-chrome] Going back to %s"
                         (nth ref-man-chrome-history-position ref-man-chrome-history)))
        (ref-man-update-from-history))
    (message "[ref-man-chrome] Cannot go back any further")))

(defun ref-man-chrome-forward-url ()
  (interactive)
  (if (> ref-man-chrome-history-position 0)
      (progn
        (decf ref-man-chrome-history-position)
        (message (format "[ref-man-chrome] Going forward to %s"
                         (nth ref-man-chrome-history-position ref-man-chrome-history)))
        (ref-man-update-from-history))
    (message "[ref-man-chrome] Cannot go forward any further")))

(defun ref-man-chrome-copy-url (url)
  (interactive (list (shr-url-at-point current-prefix-arg)))
  (unless url
    (setq url ref-man-chrome--page-url))
  (when (and (not (string-prefix-p "javascript" url)) (not (string-prefix-p "http" url)))
    (setq url (concat ref-man-chrome-root-url (string-remove-prefix "/" url))))
  (setq url (url-encode-url url))
  (kill-new url)
  (message "[ref-man-chrome] Copied %s" url))

;; TODO: These are placeholders
(defun ref-man-chrome-download ()
  (interactive)
  (message "[ref-man-chrome] Does nothing for now"))

(defun ref-man-chrome-list-histories ()
  (interactive)
  (message "[ref-man-chrome] Does nothing for now"))

(defun ref-man-chrome-list-buffers ()
  (interactive)
  (message "[ref-man-chrome] Does nothing for now"))

(defun ref-man-chrome-add-bookmark ()
  (interactive)
  (message "[ref-man-chrome] Does nothing for now"))

(defun ref-man-chrome-list-bookmarks ()
  (interactive)
  (message "[ref-man-chrome] Does nothing for now"))

(defun ref-man-chrome-view/download-pdf ()
  (interactive)
  (message "[ref-man-chrome] Does nothing for now"))

;; TODO: Check why code to put text property is not working.
;; CHECK: Why is this interactive?
(defun ref-man-chrome-setup-buffer (&optional dont-save)
  (interactive)
  (unless dont-save
    (ref-man-chrome-save-history))
  (let ((inhibit-read-only t))
    (remove-overlays)
    ;; (erase-buffer)
    (save-excursion
      (let ((current nil)
            (start nil)
            (end nil))
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
(defun ref-man-clear-history ()
  "Clear all history except the current url"
  (interactive)
  (loop for x in ref-man-chrome-history
        do (unless (string= ref-man-chrome--page-url)
             (remhash x ref-man-chrome--history-table)))
  (setq ref-man-chrome-history `(,ref-man-chrome--page-url))
  (setq ref-man-chrome-history-position 0))

;; FIXME: these should be moved from here into some config
(global-set-key (kbd "C-c e c") 'ref-man-chrome-gscholar)
(defun ref-man-chrome-gscholar (url)
  "Mostly derived from `eww' and `eww--dwim-expand-url'"
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
			  (string-match eww-local-regex url))))
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

(defun ref-man-chrome-current-gscholar-query-with-range (range)
  "For the current Google Scholar page, set the year range for
the same search query.

Range is to be entered as a space separate strings and must be
valid 4 digit years. MIN first and MAX second. If MAX is omitted
then MIN is the year from which to show results.

E.g., if results from 2010-2018 then \"2010 2018\" should be
entered on the prompt. If \"2010\" alone is entered then results
from 2010 onwards will be displayed."
  (interactive (list (if (eq major-mode 'ref-man-chrome-mode)
                         (read-from-minibuffer "Enter the range (MIN MAX): ")
                       (message "Not in ref-man-chrome-mode")
                       nil)))
  (when range
    (setq range (split-string range " "))
    (let ((low (car range))
          (hi (nth 1 range))
          (url (string-join (remove-if (lambda (x) (or (string-match-p "as_ylo=" x)
                                                       (string-match-p "as_yhi=" x)))
                                       (split-string ref-man-chrome--page-url "&")) "&")))
      (if hi
          (ref-man-chrome-gscholar
           (concat url (format "&as_yhi=%s" hi) (format "&as_ylo=%s" low)))
        (ref-man-chrome-gscholar
         (concat url (format "&as_ylo=%s" low)))))))

;; FIXME: Why do I have to call it twice for it to load correctly?
;;        Either a timing issue or a callback issue.
;;        
;; CHECK: Do I still call it twice?
(defun ref-man-chrome-search-heading-on-gscholar ()
  "Searches for the current heading in google scholar in
eww. Stores the buffer and the position from where it was called."
  (interactive)
  (unless ref-man-chrome--initialized-p
    (ref-man-chrome-init))
  (if (eq major-mode 'org-mode)
      (progn
        (setq ref-man--org-gscholar-launch-point (point)) ; CHECK: must be at heading?
        (setq ref-man--org-gscholar-launch-buffer (current-buffer))
        (save-excursion
          (let ((query-string (replace-regexp-in-string ":\\|/" " "
                                                        (substring-no-properties (org-get-heading t t)))))
            (message (concat "[ref-man-chrome] Fetching " query-string))
            (ref-man-chrome--set-location-fetch-html
             (concat "https://scholar.google.com/scholar?q="
                     (replace-regexp-in-string " " "+" query-string))))))
    (message "[ref-man-chrome] Not in org-mode")))
;; END ref-man-chrome commands

(defun ref-man-chrome-links-at-point ()
  "Return list of URIs, if any, linked at point."
  (remq nil
	(list (get-text-property (point) 'shr-url)
	      (get-text-property (point) 'image-url))))

;; I'm calling it ref-man-chrome- sub package
;; A free open port can be chosen randomly and chromium or chromium-browser can start
;; Check for "google-chrome", "chromium", "chromium-browser"
;; "which [chrome]" doesn't always correspond to the file in the running process
(defun ref-man-chrome--which-chromium ()
  (setq ref-man-chrome-command
        (replace-regexp-in-string
         "\n" "" 
         (cond ((not (string-equal "" (shell-command-to-string "which chromium-browser")))
                (shell-command-to-string "which chromium-browser"))
               ((not (string-equal "" (shell-command-to-string "which chromium")))
                (shell-command-to-string "which chromium"))
               ((not (string-equal "" (shell-command-to-string "which google-chrome")))
                (shell-command-to-string "which google-chrome"))
               (t nil)))))

(defun ref-man-chrome--chromium-running ()
  (let* ((chrome (file-name-nondirectory (ref-man-chrome--which-chromium)))
         (grep-string (concat "ps -ef | grep " chrome))
         (text (replace-regexp-in-string ".*grep.*" ""
                                         (shell-command-to-string grep-string))))
    (string-match-p chrome text)))

(defun ref-man-chrome--find-open-port ()
  "Finds the next open port from
`ref-man-chrome-port-number-start-from' in case
`ref-man-chrome-port-number-start-from' is being used by another
process. Uses `find-open-port'"
  (find-open-port ref-man-chrome-port-number-start-from))

(defun ref-man-chrome--process-helper (headless data-dir port)
  (message "[ref-man-chrome] Starting Chromium process")
  (if ref-man-use-proxy
      (if headless
          (start-process "chromium" "*chromium*" (ref-man-chrome--which-chromium)
                         (format "--proxy-server=socks://localhost:%d" ref-man-proxy-port)
                         "--headless" (concat "--user-data-dir=" data-dir)
                         (format "--remote-debugging-port=%s" port))
        (start-process "chromium" "*chromium*" (ref-man-chrome--which-chromium)
                       (format "--proxy-server=socks://localhost:%d" ref-man-proxy-port)
                       (concat "--user-data-dir=" data-dir)
                       (format "--remote-debugging-port=%s" port)))
    (if headless
        (start-process "chromium" "*chromium*" (ref-man-chrome--which-chromium)
                       "--headless" (concat "--user-data-dir=" data-dir)
                       (format "--remote-debugging-port=%s" port))
      (start-process "chromium" "*chromium*" (ref-man-chrome--which-chromium)
                     (concat "--user-data-dir=" data-dir)
                     (format "--remote-debugging-port=%s" port)))))

(defun ref-man-chrome--kill-chrome-process ()
  "Kill the chrome process"
  (signal-process (get-buffer "*chromium*") 15))

(defun ref-man-chrome-start-process (headless)
  "Start a new chromium process."
  (interactive)
  (let ((command (ref-man-chrome--which-chromium))
        (port (ref-man-chrome--find-open-port))
        (data-dir ref-man-chrome-user-data-dir))
    (setq ref-man-chrome--chromium-port port)
    (if (ref-man-chrome--chromium-running)
        (if (y-or-n-p "Existing chrome process found. Kill it and start headless?")
            (progn
              (ref-man-chrome--kill-chrome-process)
              (ref-man-chrome--process-helper headless data-dir port))
          (message "[ref-man-chrome] Not starting chromium"))
      (ref-man-chrome--process-helper headless data-dir port))))

(defun ref-man-chrome-restart ()
  (interactive)
  (ref-man-chrome-shutdown)
  (ref-man-chrome-init))

(defun ref-man-chrome--check-port (port)
  (let* ((url (url-parse-make-urlobj
               "http" nil nil "localhost" port "/"))
         (url-request-method "GET")
         (url-http-attempt-keepalives nil)
         (buf (url-retrieve-synchronously url nil nil 1)))
    (with-current-buffer buf
      (when (string-match-p "headless remote" (buffer-string))
        t))))

(defun ref-man-chrome-get-json ()
  (let* ((url (url-parse-make-urlobj
               "http" nil nil "localhost" ref-man-chrome--chromium-port "/json"))
         (url-request-method "GET")
         (url-http-attempt-keepalives nil)
         (json-array-type 'list)        ; changes to list instead of vector
         (json-object-type 'plist))     ; Not sure how this effects
    ;; (buf (url-retrieve-synchronously url nil nil 1)))
    (with-current-buffer (url-retrieve-synchronously url)
      (if (not (eq 200 (url-http-parse-response)))
          (error "Unable to connect to host.")
        (goto-char (+ 1 url-http-end-of-headers))
        (json-read)))))

(defun ref-man-chrome-get-tabs ()
  (let* ((json-string (ref-man-chrome-get-json))
         (tabs (-filter (lambda (tab)
                          (and (plist-get tab :webSocketDebuggerUrl)
                               (string-equal (plist-get tab :type) "page")))
                        json-string)))
    tabs))

(defun ref-man-chrome-reset ()
  (setq ref-man-chrome--chromium-port nil)
  (setq ref-man-chrome--rpc-id 0)
  (setq ref-man-chrome--rpc-callbacks nil)
  (setq ref-man-chrome--sockets nil)
  (setq ref-man-chrome--initialized-p nil)
  (setq ref-man-chrome--tabs nil))

(defun ref-man-chrome--encode (data)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-encode data)))

(defun ref-man-chrome--decode (data)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-read-from-string data)))

(defun ref-man-chrome--next-rpc-id ()
  (setq ref-man-chrome--rpc-id (+ 1 ref-man-chrome--rpc-id))
  ref-man-chrome--rpc-id)

(defun ref-man-chrome--register-callback (id fn)
  (setq ref-man-chrome--rpc-callbacks (plist-put ref-man-chrome--rpc-callbacks id fn)))

(defun ref-man-chrome--dispatch-callback (id data)
  (let ((hook (plist-get ref-man-chrome--rpc-callbacks id)))
    (if hook
        (progn
          (when ref-man-chrome-verbose
            (message (format "Dispatching callback for %d" id)))
          (cond ((symbolp hook)
                 (run-hook-with-args hook data))
                ((listp hook)
                 (funcall hook data))
                (t (message "Not sure what to do")))
          (setq ref-man-chrome--rpc-callbacks (plist-put ref-man-chrome--rpc-callbacks id nil)))
      (when ref-man-chrome-verbose
        (message "No hook for id %d" id)))))

;; NOTE: It's called method but it's actually THE CALL
(defun ref-man-chrome-call-rpc (method socket &optional params callback)
  (let ((id (ref-man-chrome--next-rpc-id)))
    (when ref-man-chrome-verbose
      (if (string= method "Runtime.evaluate")
          (if callback
              (message (concat "[ref-man-chrome]: Method: " (plist-get params :expression)
                               " ID: " (format "%s " id) "Added Callback: " (format "%s" callback)))
            (message (concat "[ref-man-chrome]: Method: " (plist-get params :expression)
                             " ID: " (format "%s " id) "No Callback")))
        (message (concat "[ref-man-chrome]: Method: " method " ID: " (format "%s " id) " No Callback"))))
    (when callback
      (ref-man-chrome--register-callback id callback))
    (websocket-send-text
     socket
     (ref-man-chrome--encode (list :id id
                                   :method method
                                   :params params)))))

(defun ref-man-chrome--on-open (socket)
  (message "[ref-man-chrome]: Opened new websocket"))

(defun ref-man-chrome--on-close (socket)
  (message "[ref-man-chrome]: closed websocket"))
  ;; (message "[ref-man-chrome]: Closed connection to " (format "%s") tab))

(defun ref-man-chrome--on-message-added (data)
  (when data
    (let* ((-message (plist-get data :message))
           (url (plist-get message :url))
           (column (plist-get message :column))
           (line (plist-get message :line))
           (type (plist-get message :type))
           (level (plist-get message :level))
           (text (plist-get message :text)))
      (when ref-man-chrome-verbose
        (message (format "[ref-man-chrome]: %s" -message))))))

;; (defun kite-mini-on-script-parsed (data)
;;   (let ((extension? (plist-get data :isContentScript))
;;         (url (plist-get data :url))
;;         (id (plist-get data :scriptId)))
;;     (when (and (eq extension? :json-false) (not (string-equal "" url)))
;;       (add-to-list 'kite-mini-rpc-scripts (list :id id :url url)))))

;; (defun kite-mini-on-script-failed-to-parse (data)
;;   (kite-mini-console-append (format "%s" data)))

;; FIXME: What does params even do here and how I can do plist-get without a list?
(defun ref-man-chrome--on-script-parsed (params &optional id result)
  (if ref-man-chrome-verbose
      (progn
        (if params
            (message (format "[ref-man-chrome] Parsed Script\n%s" params))
          (message "[ref-man-chrome] Parsed Script. No Params"))
        (if (and id result)
            (progn
              (message (format "[ref-man-chrome] BOTH id result %s %s" id result))
              (ref-man-chrome--dispatch-callback (plist-get id)
                                                 (plist-get result)))
          (message "[ref-man-chrome] NEITHER id result")))
    (when (and id result)
      (ref-man-chrome--dispatch-callback (plist-get id)
                                         (plist-get result)))))

(defun ref-man-chrome--failed-to-parse (params)
  (if params
      (message (format "[ref-man-chrome] Failed Script\n%s" params))
    (message "[ref-man-chrome] Failed Script. No Params")))

;; FIXME: Some weird error being given saying
;;        Error (websocket): in callback `on-message': Wrong type argument: stringp, nil
(defun ref-man-chrome--on-message (socket data)
  (let* ((data (ref-man-chrome--decode (when data (websocket-frame-payload data))))
         (method (plist-get data :method))
         (params (plist-get data :params))
         (id (plist-get data :id))
         (result (plist-get data :result)))
    (pcase method
      ;; Script is parsed which is to fetch thingy
      ("Debugger.scriptParsed" (ref-man-chrome--on-script-parsed params id result))
      ;; we are getting an error in Console.messageAdded
      ("Debugger.scriptFailedToParse" (ref-man-chrome--failed-to-parse params))
      ("Console.messageAdded" (ref-man-chrome--on-message-added params))
      ;; ;; TODO: do something useful here, possibly great for REPL
      ;; ("Console.messageRepeatCountUpdated")
      ;; nil -> These are return messages from RPC calls, not notification
      (_
       (if method
           (when ref-man-chrome-verbose
             (message "Some method!"))
         (ref-man-chrome--dispatch-callback (plist-get data :id)
                                            (plist-get data :result)))))))

(defun ref-man-chrome--open-socket (url)
  (if url
      (message "[ref-man-chrome]: Opening socket for" (format "%s" url))
      (message "[ref-man-chrome]: Opening socket for no url"))
  (websocket-open url
                  :on-open #'ref-man-chrome--on-open
                  :on-message #'ref-man-chrome--on-message
                  :on-close #'ref-man-chrome--on-close))

(defun ref-man-chrome--get-socket-url-for-tab (id)
  (let* ((tabs (ref-man-chrome-get-tabs))
         (tab-ids (mapcar (lambda (x) (plist-get x :id)) tabs))
         (sockets (mapcar (lambda (x)
                            (cons (plist-get x :id) (plist-get x :webSocketDebuggerUrl)))
                          tabs))
         (indx (if ref-man-chrome--tabs
                   (+ 1 (apply 'max (mapcar 'cdr ref-man-chrome--tabs))) 0)))
    (loop for x in tab-ids
          do (progn (when (not (assoc x ref-man-chrome--tabs))
                      (push (cons x indx) ref-man-chrome--tabs)
                      (incf indx))))
    (cdr (assoc (car (rassoc id ref-man-chrome--tabs)) sockets))))

;; TODO: Check for already connected
(defun ref-man-chrome-connect (id)
  (let* ((socket-url (ref-man-chrome--get-socket-url-for-tab id))
         (socket (ref-man-chrome--open-socket socket-url)))
    (setq ref-man-chrome--sockets (plist-put ref-man-chrome--sockets id socket))
    ;; (setq ref-man-chrome--sockets (nconc ref-man-chrome--sockets (list socket)))
    (ref-man-chrome-call-rpc "Console.enable" socket)
    (ref-man-chrome-call-rpc "Debugger.enable" socket)
    (ref-man-chrome-call-rpc "Network.setCacheDisabled" socket
                        '(:cacheDisabled t))))

(defun ref-man-chrome-eval (eval-str id &optional callback)
  "Evaluate expression for either tab 0 or 1"
  (ref-man-chrome-call-rpc
   "Runtime.evaluate"                   ; method 
   (plist-get ref-man-chrome--sockets id) ; socket
   (list :expression eval-str             ; javascript code usually
         :returnByValue t)              ; params
   callback))

(defun ref-man-chrome-shutdown (&optional restart)
  "Kill the async process and reset"
  (ref-man-chrome--kill-chrome-process)
  (ref-man-chrome-reset)
  (message "Killed chromium process"))

(defun ref-man-chrome-init (&optional not-headless)
  (if ref-man-chrome--initialized-p
      (message "[ref-man-chrome] Chromium plugin was already initialized.")
    ;; FIXME: This is confusing
    (ref-man-chrome-start-process (not not-headless))
    (setq ref-man--test-var nil)
    ;; (setq my/test-loop-iter 0)
    (while (not ref-man--test-var)        
      ;; (setq my/test-loop-iter (+ 1 my/test-loop-iter))
      (condition-case ref-man--test-var
          (progn (ref-man-chrome-connect 0)
                 (setq ref-man--test-var t))
        (error nil)))
    (setq ref-man---init-0 t)
    (setq ref-man---init-1 t)
    (ref-man-chrome-eval "location.href = \"https://scholar.google.com\"" 0
                         (lambda (result)
                           (setq ref-man---init-0 nil)))
    ;; (lambda (result) (message (concat "[ref-man-chrome] Set URL of tab 0 to "
    ;;                                   (plist-get (plist-get result :result) :value)))))
    (while ref-man---init-0
      (message "[ref-man-chrome] WAITING init-0")
      (sleep-for .2))
    (ref-man-chrome-eval "window.open();" 0
                         (lambda (result)
                           (setq ref-man---init-1 nil)))
    ;; (lambda (result) (message (concat "[ref-man-chrome] Set URL of tab 0 to "
    ;;                                   (plist-get (plist-get result :result) :value)))))
    (while ref-man---init-1
      (message "[ref-man-chrome] WAITING init-1")
      (sleep-for .2))
    (ref-man-chrome-connect 1)
    (setq ref-man-chrome--initialized-p t)
    (message "ref-man-chrome initialized")))

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

(defun ref-man-chrome--page-load-check (src str)
  (and str (string-match-p str src)))

;; CHECK: Why am I not waiting for title and stuff here?
(defun ref-man-chrome--fetch-html-no-render (tab-id)
  "Fetch the html from the specified `tab-id' but don't set any
of the history or data variables. Basically get the data from the
tab."
  (setq ref-man-chrome--nr-page-source nil)
  (setq ref-man-chrome--nr-page-title nil)
  (setq ref-man-chrome--nr-page-url nil)
  ;; (while (or (not ref-man-chrome--nr-page-title)
  ;;            (string= "" ref-man-chrome--nr-page-title))
  ;;   (ref-man-chrome-eval "document.title" tab-id
  ;;                        (lambda (result)
  ;;                          (setq ref-man-chrome--nr-page-title
  ;;                                (plist-get (plist-get result :result) :value))))
  ;;   (sleep-for .2)
  ;;   (message "WAITING page-title"))
  (ref-man-chrome-eval "document.location" tab-id
                       (lambda (result)
                         (setq ref-man-chrome--nr-page-url
                               (plist-get (plist-get (plist-get result :result) :value) :href))))
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
    (message (concat "[ref-man-chrome] WAITING fetch-html"))
    (sleep-for .2)))                    ; sync call

;; FIXME: This should setup all the global ref-man variables
(defun ref-man-chrome--fetch-html (tab-id &optional no-pop)
  "Fetch the html from the specified `tab-id'. `no-pop' directs
to don't pop to the buffer and `no-render' means do not render
html but only return the source; it also implies `no-pop'"
  (if no-pop
      (progn
        ;; CHECK: Why's `ref-man-chrome--fetch-no-pop' required?
        (setq ref-man-chrome--fetch-no-pop t))
    (setq ref-man-chrome--fetch-no-pop nil))
  (setq ref-man-chrome--page-source nil)
  (setq ref-man-chrome--page-title nil)
  (setq ref-man-chrome--page-url nil)
  (while (or (not ref-man-chrome--page-title)
             (string= "" ref-man-chrome--page-title))
    (ref-man-chrome-eval "document.title" tab-id
                         (lambda (result)
                           (setq ref-man-chrome--page-title
                                 (plist-get (plist-get result :result) :value))))
    (sleep-for .2)
    (message "[ref-man-chrome] WAITING page-title"))
  (ref-man-chrome-eval "document.location" tab-id
                       (lambda (result)
                         (setq ref-man-chrome--page-url
                               (plist-get (plist-get (plist-get result :result) :value) :href))))
  (while (or (not ref-man-chrome--page-title)
             (not ref-man-chrome--page-url)
             (not (and ref-man-chrome--page-source
                       (string-match-p "</body>" ref-man-chrome--page-source))))
    (ref-man-chrome-eval "document.documentElement.innerHTML" tab-id
                       (lambda (result)
                         (setq ref-man-chrome--page-source
                               (plist-get (plist-get result :result) :value))))
    (message "[ref-man-chrome] WAITING fetch-html")
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
                           (message "[ref-man-chrome] HERE")
                           (with-current-buffer (get-buffer "*chrome*")
                             (setq-local buffer-read-only t)
                             (ref-man-chrome-mode))
                           (message "[ref-man-chrome] HERE 2")
                           (if ref-man-chrome--fetch-no-pop
                               (message "[ref-man-chrome] Not going to pop to CHROME")
                             (message "[ref-man-chrome] WILL pop to CHROME")
                             (pop-to-buffer (get-buffer "*chrome*")))))))

(defun ref-man-update-from-history ()
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
  "Display the HTML rendering of the current buffer."
  (interactive (list (current-buffer)))
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
  "Go to URL in the chrome process and fetch the html after a
delay of 1 second."
  (setq ref-man--tab-id (if tab-id tab-id 0))
  (let ((call-str (format "location.href = \"%s\"" url)))
    (setq ref-man-chrome--fetched-sentinel t)
    (ref-man-chrome-eval call-str ref-man--tab-id
                         (lambda (result)
                           (setq ref-man-chrome--fetched-sentinel nil)))
    (while ref-man-chrome--fetched-sentinel
      (message "[ref-man-chrome] WAITING set-location")
      (sleep-for .2))
    ;; TODO: This should be only for EXTRA verbose
    ;; (when ref-man-chrome-verbose
    ;;   (message (format "Result of fetch call %s" (plist-get (plist-get result :result) :value))))
    (if no-render
        (progn
          (message "[ref-man-chrome] Will not render only fetch")
          (ref-man-chrome--fetch-html-no-render ref-man--tab-id)
          (list :title ref-man-chrome--nr-page-title
                :url ref-man-chrome--nr-page-url
                :source ref-man-chrome--nr-page-source))
      (ref-man-chrome--fetch-html ref-man--tab-id no-pop))))

(defun ref-man-chrome-save-history ()
  (plist-put ref-man-chrome-data :point (point))
  (plist-put ref-man-chrome-data :text ref-man-chrome--page-source)
  (plist-put ref-man-chrome-data :title ref-man-chrome--page-title)
  (plist-put ref-man-chrome-data :url ref-man-chrome--page-url)
  (puthash ref-man-chrome--page-url (copy-list ref-man-chrome-data) ref-man-chrome--history-table)
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
    ;; (loop for x in (cdr tail)
    ;;       do (setq ref-man-chrome-history-data
    ;;                (delete (assoc-string x ref-man-chrome-history-data)
    ;;                        ref-man-chrome-history-data)))
    ;; NOTE: This will delete x but not required since VALUE is replaced with puthash earlier
    ;; (loop for x in (cdr tail)
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


(provide 'ref-man-chrome)
