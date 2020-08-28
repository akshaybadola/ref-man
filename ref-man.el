;;; ref-man.el --- Manage bibliographic references and associated documents in  emacs. Integrates with org-mode to fetch, retrieve and manage the documents and metadata. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019,2020
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Wednesday 24 June 2020 09:16:15 AM IST>
;; Keywords:	pdfs, references, bibtex, org-mode, eww
;; Version:     0.1
;; Package-Requires: ((async "1.9.4") (org "9.1.9") (biblio-core "0.2.1") (gscholar-bibtex "0.3.1") (websocket "1.12") (dash "2.17.0") (dash-functional "1.2.0") (bind-key "2.4"))

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
;; TODO

;;; Code:

;; TODO: Should make ref-man minor mode

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

(defconst ref-man-home-dir (file-name-directory load-file-name)
  "Home or install directory for ref-man.")

(defconst ref-man-version "0.1"
  "`ref-man' version number.")

(require 'ref-man-core)
(require 'ref-man-chrome)

(unless (ref-man--python-process-running-p)
  (ref-man-start-python-process))

(defun ref-man-try-start-science-parse-server ()
    (let* ((java (shell-command-to-string "which java"))
       (has-java (not (string-match-p "no java" java))))
  ;; TODO: Check if already running
  (if has-java
    (if (and ref-man-science-parse-jar-file ref-man-science-parse-server-port)
        (if (y-or-n-p "[ref-man] Start Science Parse Server? ")
            (progn
              (unless (string-match-p "science-parse" (shell-command-to-string  "ps -ef | grep java"))
                (start-process "science-parse" "*science-parse*"
                               "java" "-Xmx6g" "-jar" ref-man-science-parse-jar-file)
                ;; (async-start-process "science-parse"
                ;;                      "java" nil "-Xmx6g" "-jar" ref-man-science-parse-jar-file)
                (message "[ref-man] Trying to start server. This may take some time"))
              ;; TODO: This doesn't work, check should be done later
              (if (string-match-p "Usage"
                                  (shell-command-to-string
                                   (format "curl -s localhost:%d" ref-man-science-parse-server-port)))
                  (message "[ref-man] Established connection to server successfully")
                (message "[ref-man] ERROR! Check connections")))
          (message "[ref-man] Not starting Science Parse Server"))
      (message "[ref-man] Science Parse Jar File not given"))
    (message "[ref-man] java not found"))))

(ref-man-try-start-science-parse-server)

(provide 'ref-man)

;;; ref-man.el ends here
