;;; easy-tabulated-list.el --- Simplify usage for `tabulated-list'  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-08-23 17:15:12

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Simplify usage for `tabulated-list'.
;; Keyword: tabulated list table usage simplify
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/jcs-elpa/easy-tabulated-list

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
;; Simplify usage for `tabulated-list'.
;;

;;; Code:

(require 'tabulated-list)

(defun easy-tabulated-list-form-entries-list (list-data)
  "Form entries from LIST-DATA.
LIST-DATA is a list like the data type below.

'(a b c d e f g h i j k l m)"
  (let ((len (length tabulated-list-format))
        (list-2-data '()) (lst '()) (cnt 0))
    (unless (numberp len)
      (user-error "[ERROR] Can't form entries without a valid `tabulated-list-format' is set"))
    (dolist (item list-data)
      (when (and (= (% cnt len) 0) (not (= cnt 0)))
        (push (reverse lst) list-2-data)
        (setq lst '()))
      (push item lst)
      (setq cnt (1+ cnt)))
    (push (reverse lst) list-2-data)
    (easy-tabulated-list-form-entries-list-2 (reverse list-2-data))))

(defun easy-tabulated-list-form-entries-list-2 (list-2-data)
  "Form entries from LIST-2-DATA.
LIST-2-DATA is a 2 dimensional list like the data type below.

'((a b c e f g) (h i j k l m))"
  (let ((entries '()) (id-count 0))
    (dolist (inner-lst list-2-data)
      (let ((new-entry '()) (new-entry-value '()))
        (dolist (item inner-lst) (push item new-entry-value))
        (setq new-entry-value (reverse new-entry-value))
        (progn  ; Formal
          (push (vconcat new-entry-value) new-entry)  ; Turn into vector.
          (push (number-to-string id-count) new-entry)  ; ID
          (push new-entry entries))
        (setq id-count (1+ id-count))))
    entries))

;;;###autoload
(defun easy-tabulated-list-form-entries (data)
  "Form entries from DATA.
DATA can either be the following data type.
  - vector
  - list
  - 2 dimensional list
  - 2 dimensional vector"
  (let (inner-item result)
    (cond ((listp data)
           (setq inner-item (nth 0 data))
           (cond ((listp inner-item)  ; 2 dimensional list
                  (setq result (easy-tabulated-list-form-entries-list-2 data)))
                 (t  ; list
                  (setq result (easy-tabulated-list-form-entries-list data)))))
          ((vectorp data)
           )
          (t
           (user-error "[ERROR] Unknown data type to form entries: %s" data)))
    result))

(provide 'easy-tabulated-list)
;;; easy-tabulated-list.el ends here
