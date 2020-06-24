;; ref-man.el --- Manage bibliographic references and associated documents in  emacs. Integrates with org-mode to fetch, retrieve and manage the documents and metadata. ;;; -*- lexical-binding: t; -*-

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


(defcustom ref-man-science-parse-server-port 8080
  "Server port on which to communicate with science parse server"
  :type 'integer
  :group 'ref-man)

(defcustom ref-man-science-parse-jar-file nil
  "Jar file which will launch the Science Parse Server. This
should be compiled from source.
See URL `https://github.com/allenai/science-parse' for details"
  :type 'file
  :group 'ref-man)

;; TODO: This should warn that java is required
;; FIXME: This thing should wait while it starts
(when (and ref-man-science-parse-jar-file ref-man-science-parse-server-port)
  (if (y-or-n-p "Start Science Parse Server?")
      (progn
        (unless (string-match-p "science-parse" (shell-command-to-string  "ps -ef | grep java"))
          (async-start-process "science-parse" "java" nil "-Xmx6g" "-jar" ref-man-science-parse-jar-file)
          (message "[ref-man] Trying to start server. This may take some time"))
        (if (string-match-p "Usage"
                            (shell-command-to-string (concat "curl -s localhost:" server-port)))
            (message "[ref-man] Established connection to server successfully")
          (message "[ref-man] ERROR! Check connections")))
    (message "[ref-man] Not starting Science Parse Server")))

(require 'ref-man-core)
(require 'ref-man-chrome)
(provide 'ref-man)
