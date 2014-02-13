;;; chrome-cli.el --- Chrome CLI Emacs interface

;; Copyright (C) 2014 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Keywords: chrome, browser
;; URL: http://github.com/rejeep/chrome-cli.el

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(defvar chrome-cli-executable nil
  "Path to chrome-cli executable.")

(defun chrome-cli--command (&rest args)
  "Run chrome-cli command with ARGS.

Return value is a list with each row in the result as an item in
the list."
  (with-temp-buffer
    (let* ((chrome-cli (executable-find (or chrome-cli-executable "chrome-cli")))
           (exit-code (apply 'call-process (append (list chrome-cli nil t nil) args))))
      (if (= exit-code 0)
          (s-lines (s-trim (buffer-string)))
        (error "Something went wrong: %s" (buffer-string))))))

(defun chrome-cli-windows ()
  "Return list of windows.

Each item in the list is a plist with id and name."
  (let (windows)
    (-each (chrome-cli--command "list" "windows")
      (lambda (line)
        (let ((matches (s-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line)))
          (push
           (list :id (nth 1 matches)
                 :name (nth 2 matches))
           windows))))
    windows))

(defun chrome-cli-tabs (&optional window-id)
  ""
  (let (tabs)

    (-each
        ;; TODO: REJECT
        (--select
         (if window-id
             (equal window-id (plist-get it :id))
           t
           )
         (chrome-cli-windows))
      (lambda (window)
        (let (ids titles urls (window-id (plist-get window :id)))
          (-each (chrome-cli--command "list" "tabs" "-w" window-id)
            (lambda (line)
              (let ((matches (s-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line)))
                (push (nth 1 matches) ids)
                (push (nth 2 matches) titles))))
          (-each (chrome-cli--command "list" "links" "-w" window-id)
            (lambda (line)
              (let ((matches (s-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line)))
                (push (nth 2 matches) urls))))
          (-map-indexed
           (lambda (index id)
             (push (list :id id
                         :title (nth index titles)
                         :url (nth index urls))
                   tabs))
           ids))))
    tabs))
;; (chrome-cli-tabs)
;; (chrome-cli-tabs "38")

;; chrome-cli info  (Print info for active tab)
;; chrome-cli info -t <id>  (Print info for specific tab)
(defun chrome-cli-tab (&optional tab-id)
  ""
  (let ((lines
         (if tab-id
             (chrome-cli--command "info" "-t" tab-id)
           (chrome-cli--command "info"))))
    (let (id title url loading)
      (-each lines
        (lambda (line)
          (cond ((s-starts-with? "Id:" line)
                 (setq id (cadr (s-split " " line))))
                ((s-starts-with? "Title:" line)
                 (setq title (cadr (s-split " " line))))
                ((s-starts-with? "Url:" line)
                 (setq url (cadr (s-split " " line))))
                ((s-starts-with? "Loading:" line)
                 (setq loading (string= (cadr (s-split " " line)) "Yes"))))))
      (list :id id
            :title title
            :url url
            :loading loading))))
;; (chrome-cli-tab)
;; (chrome-cli-tab "1813")

(setq chrome-cli-executable "~/bin/chrome-cli")

;; chrome-cli open <url>  (Open url in new tab)
;; chrome-cli open <url> -n  (Open url in new window)
;; chrome-cli open <url> -i  (Open url in new incognito window)
;; chrome-cli open <url> -t <id>  (Open url in specific tab)
;; chrome-cli open <url> -w <id>  (Open url in new tab in specific window)
(defun chrome-cli-open (url &rest args)
  ""
  (let ((tab-id (plist-get args :tab-id))
        (window-id (plist-get args :window-id))
        (new-window (plist-get args :new-window))
        (new-incognito-window (plist-get args :new-incognito-window)))
    (cond (tab-id
           (chrome-cli--command "open" url "-t" tab-id))
          (window-id
           (chrome-cli--command "open" url "-w" window-id))
          (new-window
           (chrome-cli--command "open" url "-n"))
          (new-incognito-window
           (chrome-cli--command "open" url "-i"))
          (t
           (chrome-cli--command "open" url)))))
;; (chrome-cli-open "http://tuxicity.se")

;; chrome-cli close  (Close active tab)
;; chrome-cli close -w  (Close active window)
;; chrome-cli close -t <id>  (Close specific tab)
;; chrome-cli close -w <id>  (Close specific window)
(defun chrome-cli-close (&rest args)
  ""
  (let ((window (plist-get args :window))
        (tab-id (plist-get args :tab-id))
        (window-id (plist-get args :window-id)))
    (cond (tab-id
           (chrome-cli--command "close" "-t" tab-id))
          (window-id
           (chrome-cli--command "close" "-w" window-id))
          (window
           (chrome-cli--command "close" "-w"))
          (t
           (chrome-cli--command "close")))))
;; (chrome-cli-close)

;; chrome-cli reload  (Reload active tab)
;; chrome-cli reload -t <id>  (Reload specific tab)
(defun chrome-cli-reload (&optional tab-id)
  ""
  (if tab-id
      (chrome-cli--command "reload" "-t" tab-id)
    (chrome-cli--command "reload")))

;; chrome-cli back  (Navigate back in active tab)
;; chrome-cli back -t <id>  (Navigate back in specific tab)
(defun chrome-cli-back (&optional tab-id)
  ""
  (if tab-id
      (chrome-cli--command "back" "-t" tab-id)
    (chrome-cli--command "back")))

;; chrome-cli forward  (Navigate forward in active tab)
;; chrome-cli forward -t <id>  (Navigate forward in specific tab)
(defun chrome-cli-forward (&optional tab-id)
  ""
  (if tab-id
      (chrome-cli--command "forward" "-t" tab-id)
    (chrome-cli--command "forward")))

;; chrome-cli source  (Print source from active tab)
;; chrome-cli source -t <id>  (Print source from specific tab)
(defun chrome-cli-source (&optional tab-id)
  ""
  (if tab-id
      (chrome-cli--command "source" "-t" tab-id)
    (chrome-cli--command "source")))

;; chrome-cli execute <javascript>  (Execute javascript in active tab)
;; chrome-cli execute <javascript> -t <id>  (Execute javascript in specific tab)
(defun chrome-cli-execute (javascript &optional tab-id)
  ""
  (if tab-id
      (chrome-cli--command "execute" javascript "-t" tab-id)
    (chrome-cli--command "execute" javascript)))

;; chrome-cli chrome version  (Print Chrome version)
(defun chrome-cli-chrome-version ()
  ""
  (car (chrome-cli--command "chrome" "version"))
  )
;; (chrome-cli-chrome-version)

;; chrome-cli version  (Print application version)
(defun chrome-cli-version ()
  ""
  (car (chrome-cli--command "version"))
  )
(chrome-cli-version)


(provide 'chrome-cli)

;;; chrome-cli.el ends here
