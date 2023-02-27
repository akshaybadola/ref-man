;;; ref-man.el --- Manage bibliographic references and associated documents in  emacs. Integrates with org-mode to fetch, retrieve and manage the documents and metadata. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019,2020,2021,2022,2023
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Monday 27 February 2023 09:02:53 AM IST>
;; Keywords:	pdfs, references, bibtex, org-mode, eww
;; Version:     0.7.27
;; Package-Requires: ((a "0.1.1") (async "1.9.4") (org "9.5") (biblio-core "0.2.1") (gscholar-bibtex "0.3.1")
;;                    (find-file-in-project "6.2.1") (websocket "1.12") (dash "2.18.0") (bind-key "2.4")
;;                    (org-ref "1.1.1") (yaml "0.1.0"))

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
;; Emacs library to manage bibliography having tight integration with org-mode
;; and eww.  Grew out of a frustration to effectively manage downloaded pdfs,
;; notes, export/import to bib and document generation.

;; Very much a work in progress, it's similar in functionality to
;; `org-ref'.  When I had started writing this, (I don't exactly recall) either I
;; didn't know of its existence or felt that it lacked a few features that I
;; needed.  So the project evolved as such.

;; Features:
;; - Bibliography Management with `org-mode'.  Import/export bib files
;;   to org mode entries.  The data is stored in the org heading PROPERTIES table.
;; - Auto download from supported remote URLs
;;   See `ref-man-url-get-pdf-link-helper' for the list of supported sites.
;; - Auto fetch bibliography from DBLP (or other specified source)
;;   Currently Google Scholar, DBLP and Crossref are supported.
;;   - Convert the references for a paper to an org buffer and display for easy
;;     inspection and marking
;;   - Update the properties from the fetched bib data.
;; - Strong Integration with remote servers for PDF, bibliography and other
;;   metadata extraction
;;   - arxiv.org
;;   - semanticscholar.org
;;   - scholar.google.com

;; Details
;; - Use chromium (or google chrome) to avoid google's annoying "Prove you're not a
;;   robot" message for scholar.google.com
;;   - Uses the debug adapter to route pages through chromium and display in an `eww'
;;     like buffer
;;   - Helps if you're logged in with a gmail id.

;;; Code:

;; TODO: Should make ref-man minor mode
;; TODO: Have to set debug levels. The info is way too much

(defcustom ref-man-sp-forward-remote-port nil
  "Whether to forward port from a remote ssh server."
  :type 'string
  :group 'ref-man)

(defcustom ref-man-science-parse-remote-host "localhost"
  "Hostname or address where the science parse server is running."
  :type 'string
  :group 'ref-man)

(defcustom ref-man-science-parse-server-port 8080
  "Server port on which to communicate with science parse server."
  :type 'integer
  :group 'ref-man)

(defcustom ref-man-science-parse-jar-file nil
  "Jar file which will launch the Science Parse Server.
This should be compiled from source.
See URL `https://github.com/allenai/science-parse' for details"
  :type 'file
  :group 'ref-man)

(defcustom ref-man-use-chrome-for-search nil
  "Whether chromium should be used for searching instead of eww."
  :type 'boolean
  :group 'ref-man)

(defconst ref-man-home-dir (file-name-directory load-file-name)
  "Home or install directory for `ref-man'.")

(require 'ref-man-core)
(require 'ref-man-chrome)
(require 'ref-man-remote)
(require 'ref-man-py)
(require 'ref-man-export)

(defun ref-man-init-dirs ()
  (seq-do (lambda (x)
            (when (and x (not (f-exists-p x)))
              (make-directory x)))
          (list ref-man-data-root-dir ref-man-org-store-dir
                ref-man-documents-dir ref-man-extra-documents-dirs
                ref-man-py-data-dir)))
(ref-man-init-dirs)

(unless (ref-man-py-process-running)
  ; (ref-man-chrome-init nil t)                 ; start chromium headless first
  (ref-man-py-start-server))

(ref-man-remote-load-public-links-cache)
;; FIXME: This throws an error if python-server is not running
;; (ref-man-remote-update-links-cache)

(defun ref-man-science-parse-server-running-p (&optional show-msg)
  (let ((port ref-man-science-parse-server-port))
    (if (string-match-p "Usage"
                        (shell-command-to-string (format "curl -s localhost:%d" port)))
        (progn (when show-msg
                 (message "[ref-man] Science Parse server is running"))
               t)
      (message "[ref-man] ERROR! Check connections")
      nil)))

(defun ref-man-science-parse-try-forward-remote-ssh-port (&optional show-msg)
  (let ((host ref-man-sp-forward-remote-port)
        (port ref-man-science-parse-server-port))
    (unless (and (string-empty-p host)
                 (string-match-p host (shell-command-to-string  "ps -ef | grep ssh")))
      (async-start-process "science-parse" "ssh" nil "-N" "-L"
                           (format  "%d:localhost:%d" port port) host)
      (ref-man-science-parse-server-running-p show-msg))))

(defun ref-man-science-parse-local-server-running-p (&optional show-msg)
  "Check if the Science Parse server running.
Return \\='external if server is running but outside Emacs and
\\='internal if running inside Emacs,  nil otherwise."
  (let* ((java-proc (string-match-p "science-parse"
                                   (shell-command-to-string  "ps -ef | grep java")))
        (buf-proc (get-buffer-process "*science-parse*"))
        (proc (cond ((and java-proc (not buf-proc))
               'external)
              ((and java-proc buf-proc)
               'internal)
              (t nil))))
    (when proc
      (if (string-match-p "Usage"
                          (shell-command-to-string
                           (format "curl -s localhost:%d" ref-man-science-parse-server-port)))
          (progn (when show-msg
                   (message "[ref-man] Established connection to server successfully"))
                 proc)
        (message "[ref-man] Waiting for server to start up") 'waiting))))

(defun ref-man-kill-science-parse-process ()
  "Kill the Science Parse process."
  (interactive)
  (signal-process (get-buffer "*science-parse*") 15))

(defun ref-man-try-start-local-science-parse-server ()
  "Try to start the Science Parse server."
  (interactive)
  (let* ((status (ref-man-science-parse-local-server-running-p))
         (java (shell-command-to-string "which java"))
         (has-java (not (string-match-p "no java" java))))
    (if status status
      (if has-java
          (if (and ref-man-science-parse-jar-file ref-man-science-parse-server-port)
              (if (y-or-n-p-with-timeout "[ref-man] Science Parse Server not runing.  Start it? " 3 nil)
                  (progn
                    (unless (string-match-p "science-parse" (shell-command-to-string  "ps -ef | grep java"))
                      (start-process "science-parse" "*science-parse*"
                                     "java" "-Xmx6g" "-jar" ref-man-science-parse-jar-file)
                      (message "[ref-man] Trying to start server. This may take some time"))
                    (ref-man-science-parse-server-running-p t))
                (message "[ref-man] Not starting Science Parse Server"))
            (message "[ref-man] Science Parse Jar File not given"))
        (message "[ref-man] java not found")))))
(defalias 'ref-man-start-local-science-parse-server 'ref-man-try-start-local-science-parse-server)
(defalias 'ref-man-try-start-science-parse-server 'ref-man-try-start-local-science-parse-server)

(provide 'ref-man)

;;; ref-man.el ends here
