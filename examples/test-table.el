;;; test-table.el --- Example of how to use package `easy-tabulated-list'  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-11-23 20:35:08

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Example of how to use package `easy-tabulated-list'.
;; Keyword:
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/jcs090218/test-table

;; This file is NOT part of GNU Emacs.

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
;; Example of how to use package `easy-tabulated-list'.
;;

;;; Code:

(defvar-local test-table-format
  (vector (list "Name" 20 t) (list "Version" 8 t) (list "Status" 10))
  "Format that you consider changing it by user's actions.")

(defvar-local test-table-data '()
  "The table data you will consider changing it by user's actions.")

(defun test-table--refresh ()
  "This is the core usage of `easy-tabulated-list'."
  (easy-tabulated-list-make test-table-format
                            (easy-tabulated-list-form-entries test-table-data)
                            :padding 1)
  (tabulated-list-revert))

;; Define your `tabulated-list' major mode.
(define-derived-mode test-table-mode tabulated-list-mode
  "test-table-mode"
  "Major mode for `test-table'."
  (test-table--refresh))

;;;###autoload
(defun test-etny ()
  "This is the entry point to create `tabulated-list' buffer."
  (interactive)
  (pop-to-buffer "test-table")  ; jump to the buffer.
  (test-table-mode))  ; initialize as `test-table'.

(provide 'test-table)
;;; test-table.el ends here
