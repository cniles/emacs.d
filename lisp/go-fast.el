;;; go-fast.el --- Extensions for working with Go    -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Craig Niles

;; Author: Craig Niles <craigniles@Craig-Niles-MacBook-Pro.local>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:



(provide 'go-fast)

(defun string--join (l d) (mapconcat 'identity l d))

(defun go-run ()
  "Run the current Go buffer"
  (interactive)
  (message (async-shell-command
	    (string--join (list "go" "run" buffer-file-name) " "))))

;;; go-fast.el ends here
