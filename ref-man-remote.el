;;; ref-man-remote.el --- Components for `ref-man' to work with remote servers, like mail and remote storage. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019,2020,2021
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
;; Components for interfacing remote servers like mail and remote
;; storage.  Provides custom functions to use with `mu4e' to export `org-mode'
;; buffers as mail and attach files as remote links automatically.
;;
;; Remote storage management requires rclone to be available.  See URL
;; https://rclone.org/.  Not all remote servers may support generating public
;; links.  Refer to rclone documentation.

;;; Code:

(require 'seq)
(require 'f)
(require 'dash)
(require 'cl-lib)

(unless (featurep 'ref-man-util)
  (require 'ref-man-util))

(defcustom ref-man-remote-documents-dir ""
  "Remote documents dir which will sync with `ref-man-documents-dir'."
  :type 'string
  :group 'ref-man)

;; FIXME: Not sure what this was for, but it's not used right now.
(defcustom ref-man-mail-uploads-dir ""
  "Remote dir where mail attachments will be uploaded."
  :type 'string
  :group 'ref-man)

(defcustom ref-man-public-links-cache-file
  (expand-file-name "~/.ref-man/remote-links-cache")
  "Cache of public links to remote documents for sharing easily."
  :type 'file
  :group 'ref-man)

(defvar ref-man-public-links-cache nil
  "Hash table mapping local files to remote cache.
Maps files from `ref-man-documents-dir' to
`ref-man-remote-documents-dir'.")

(defvar ref-man-python-server-port)     ; from `ref-man-core'

(defun ref-man-remote-load-public-links-cache ()
  "Load the existing cache from disk if defined."
  (interactive)
  (unless (or (string-empty-p ref-man-public-links-cache-file)
              (string-empty-p ref-man-remote-documents-dir))
    (setq ref-man-public-links-cache (make-hash-table :test 'equal))
    (seq-do (lambda (x)
              (let ((split (split-string x ";")))
                (puthash (car split) (cadr split) ref-man-public-links-cache)))
            (split-string (with-current-buffer
                              (find-file-noselect ref-man-public-links-cache-file)
                            (buffer-string)) "\n" t))
    (message "[ref-man] Loaded remote links cache from disk.")))

(defun ref-man-remote-check-cache-updated ()
  "Check from python server if cache is updated."
  (let* ((buf (url-retrieve-synchronously
              (format "http://localhost:%s/cache_updated" ref-man-python-server-port) t))
         (buf-string (and buf (with-current-buffer buf (buffer-string)))))
    (cond ((string-match-p "updated cache with errors" buf-string)
           'updated-with-errors)
          ((string-match-p "updated cache" buf-string)
           'updated-all)
          (t nil))))

;; (defvar ref-man-remote-cache-update-check-timer nil
;;   "Timer for checking status of cache update.")
;; NOTE: This should be run async, as otherwise it'll block emacs
(defun ref-man-remote-update-links-cache ()
  "Update the public links cache and file.
For any files not in `ref-man-public-links-cache' generate link
from remote and update the `ref-man-public-links-cache-file' on
disk."
  (interactive)
  (unless (or (string-empty-p ref-man-public-links-cache-file)
              (string-empty-p ref-man-remote-documents-dir))
    ;; Else, update first
    (let ((buf (url-retrieve-synchronously
                (format "http://localhost:%s/update_links_cache?local_dir=%s&remote_dir=%s&cache_file=%s"
                        ref-man-python-server-port
                        ref-man-documents-dir
                        ref-man-remote-documents-dir
                        ref-man-public-links-cache-file)
                t)))
      ;; NOTE: Timer not implemented
      ;; (setq ref-man-remote-cache-update-check-timer
      ;;       (run-at-time 5 60 #'ref-man-remote-check-cache-updated))
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "\r?\n\r?\n")
        (message (buffer-substring-no-properties (point) (point-max)))))))

(defun ref-man-remote-cancel-cache-update-timers ()
  "Cancel the timers which call `ref-man-check-cache-updated'."
  (setq timer-list (-remove (lambda (x)
                              (eq (timer--function x)
                                  'ref-man-remote-check-cache-updated))
         timer-list)))

(provide 'ref-man-remote)

;;; ref-man-remote.el ends here
