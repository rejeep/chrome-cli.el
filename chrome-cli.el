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

(provide 'chrome-cli)

;;; chrome-cli.el ends here
