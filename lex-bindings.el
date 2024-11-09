;;; lex-bindings.el --- Few utils to work with lexical scope  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Keywords: 

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

(defun lex--lambda-list (lambda-list name)
  (when (/= (logand (length lambda-list) 1) 0)
    (signal 'wrong-number-of-arguments (list name (length lambda-list))))
  (let ((env))
    (while lambda-list
      (push (list (pop lambda-list) (pop lambda-list)) env))
    (nreverse env)))

(defmacro lex (varlist &rest body)
  "Bind variables according to VARLIST and then eval BODY.

VARLIST must be a list of the form:

 (variable-1 initial-form-1
  variable-2 initial-form-2
  ...
  variable-n initial-form-n)

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

Expands to `let*'."
  (declare (indent defun))
  `(let* ,(lex--lambda-list varlist 'lex)
     ,@body))

(defmacro lex-if (varlist then-form &rest else-forms)
  "Creates new variable bindings, and conditionally executes either
THEN-FORM or ELSE-FORMS. ELSE-FORMS defaults to NIL.

BINDINGS must be a list of the form:

 (variable-1 initial-form-1
  variable-2 initial-form-2
  ...
  variable-n initial-form-n)

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

If all variables were bound to true values, the THEN-FORM is executed with the
bindings in effect, otherwise the ELSE-FORM is executed with the bindings in
effect."
  (declare (indent defun))
    (let* ((bindings (lex--lambda-list varlist 'lex-if))
           (variables (mapcar #'car bindings)))
      `(let* ,bindings
         (if (and ,@variables)
             ,then-form
           (progn ,@else-forms))))) ;; use progn so it works ootb with CL too

(defmacro lex-when (varlist &rest body)
  "Create new bindings according to VARLIST, and conditionally evaluate BODY.

BINDINGS must be a list of the form:

 (variable-1 initial-form-1
  variable-2 initial-form-2
           ...
  variable-n initial-form-n)

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

If all variables were bound to true values, the THEN-FORM is executed with the
bindings in effect, otherwise the ELSE-FORM is executed with the bindings in
effect."
  (declare (indent defun))
  (let* ((bindings (lex--lambda-list varlist 'lex-when))
         (variables (mapcar #'car bindings)))
        `(let* ,bindings
           (when (and ,@variables)
             ,@body))))

(defmacro lex-while (varlist &rest body)
  "Create new bindings according to VARLIST, and conditionally evaluate BODY.

BINDINGS must be a list of the form:

 (variable-1 initial-form-1
  variable-2 initial-form-2
           ...
  variable-n initial-form-n)

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

If all variables were bound to true values, the THEN-FORM is executed with the
bindings in effect, otherwise the ELSE-FORM is executed with the bindings in
effect."
  (let* ((bindings (lex--lambda-list varlist 'lex-while))
         (variables (mapcar #'car bindings)))
        `(let* ,bindings
           (while (and ,@variables)
             ,@body))))

(put 'lex 'lisp-indent-function '(&lambda &body))
(put 'lex-if 'lisp-indent-function '(&lambda &body))
(put 'lex-when 'lisp-indent-function '(&lambda &body))
(put 'lex-while 'lisp-indent-function '(&lambda &body))

(provide 'lex-bindings)
;;; lex-bindings.el ends here
