;;; Commentary:

;; ------------------------------------------------------------------------
;; Copyright (C) Oleg Sivokon (olegsivokon@gmail.com)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ------------------------------------------------------------------------

;; This program is an attempt at translating iterate Common Lisp library
;; into Emacs Lisp.

;; i-iterate is NOT part of GNU Emacs.

;;; Versions:
;;
;;    0.0.0 - This is not usable yet.
;;

;;; Usage:
;;


;;; Code:


(require 'eieio)

(defmacro i-deferror (name message &optional aliases docstring)
  "Defines an error symbol NAME with the MESSAGE being it's `error-message'
If the third argument is a string, then it is used to set the documentation
for this symbol, else it is treated as aliases for this error. In the
later case, if there is one more argument, it is considered the documentation
string."
  (declare (indent 2))
  (let ((aliases (when (or docstring (not (stringp aliases)))
                   (if (listp aliases) aliases (list aliases))))
        (docstring (or docstring (when (stringp aliases) aliases))))
  `(progn
     (put ',name 'error-conditions '(error i-error ,@aliases))
     (put ',name 'error-message ,message)
     ,@(when docstring
         (list (list 'put (list 'quote name)
                     (list 'quote 'documentation) docstring))))))

(i-deferror i-unknown-verb "Unexpected verb `%s'"
    "Signaled when an unrecognized symbol is encountered while parsing the body
of `i-iterate' macro")

(defclass i-driver ()
  ((variables
    :initarg :variables
    :initform nil
    :type list
    :documentation "The variables this driver generates values for")
   (actions
    :initarg :actions
    :initform nil
    :type list
    :documentation "The actions applied to the variables to obtain the
next value, each function must accept single argument returning single value")
   (initial-values
    :initarg :initial-values
    :documentation "The initial values of the variables this driver operates on")
   (end-values
    :initarg :end-values
    :documentation "The end values of the variables this driver operates on")
   (exit-conditions
    :initarg :exit-conditions
    :initform nil
    :type list
    :documentation
    "Conditions to test in the main (while ...) expression")
   (catch-conditions
    :initarg :catch-conditions
    :initform nil
    :type list
    :documentation
    "Conditions to catch outside the main (while ...) loop"))
  :documentation "This class describes a single driver of `i-iterate' macro")
   
(defclass i-spec ()
  ((body
    :initarg :body
    :initform nil
    :type list
    :documentation
    "Lisp forms to execute in the iteration body")
   (result
    :initform nil
    :type list
    :documentation "The result form returned after the loop finishes")
   (gen-index
    :initform 0
    :type integer
    :documentation "Used internally when generating variable names")
   (drivers
    :initform nil
    :type list
    :documentation 
    "This slot contains the list of all drivers used in this iteration macro")
   (has-exclusive-hash-p
    :initform nil
    :type symbol
    :documentation "In case there is only one driver which iterates over
a hash-table, we can generate a (maphash ...) instead of (while ...). If this 
slot is non-nil, then `i-iterate' will generate the `maphash'."))
  :documentation "This class contains a specification of the
expansion of the `i-iterate' macro")

(defvar i-for-handlers nil
  "The table of all handlers for (for ...) driver. Use `i-add-for-handler' if you
need to add your own driver")
(unless i-for-handlers (setq i-for-handlers (make-hash-table)))

(defmacro i-add-for-handler (name arguments &rest handler)
  "Adds a handler for (for ...) driver. NAME is the name of the generated handler
and also the key in `i-for-handlers'. It can be a single symbol or a list.
ARGUMENTS is the argument list, which looks like this: (SPEC DRIVER EXPRESSION), 
the first argument will be bound to the instance of `i-spec', creating this driver,
the second argument will be bound to the instance of `i-driver' used to handle
this (for ...) expression, the last argument is the list of arguments following 
the for symbol.
HANDLER is the body of the generated function."
  (declare (indent 2))
  (if (consp name)
      `(let ((names ',name)
             (handler (lambda (,@arguments &rest rest) ,@handler)))
         (while names
           (puthash (car names) handler i-for-handlers)
           (setq names (cdr names))))
    `(puthash ',name (lambda (,@arguments &rest rest) ,@handler) i-for-handlers)))

(i-add-for-handler (uprform from downfrom) (spec driver exp)
  (destructuring-bind (var verb begin target limit how iterator &optional (op '<=))
      exp
    ;; TODO: need to check for constant expressions in iterator and limit
    ;; to possibly avoid generating extra vairables.
    (oset driver variables (list (list var begin)))
    (oset driver actions
          (list
           (list
            (cond
             ((member verb '(from upfrom))
              'incf)
             ((eql verb 'downfrom)
              (setq op '>=)
              'decf))
            var
            (when how
              (let ((sym (i-gensym spec)))
                (oset driver variables
                      (cons (list sym iterator) (oref driver variables)))
                sym)))))
    (setq op
          (cond
           ((eql target 'to) op)
           ((eql target 'downto) '>=)
           ((eql target 'below) '<)
           ((eql target 'upto) '>)
           ((null target) nil)
           (t (signal 'i-unknown-verb target))))
    (when op
      (let ((sym (i-gensym spec)))
        (oset driver variables
              (cons (list sym limit) (oref driver variables)))
        (oset driver exit-conditions
              (list (list op var sym)))))))

(i-add-for-handler in (spec driver exp)
  (destructuring-bind (var verb iterated-list how iterator)
      exp
    ;; TODO: need to check for constant expressions in iterator and limit
    ;; to possibly avoid generating extra vairables.
    (let ((i-list (i-gensym spec))
          (i-iterator (when iterator (i-gensym spec))))
      (oset driver variables
            (if iterator
                (append (list (list i-list iterated-list))
                        (list (list i-iterator iterator))
                        (oref driver variables))
              (cons (list i-list iterated-list)
                    (oref driver variables))))
      (oset driver actions
            (cond
             ((and (consp var) (consp (cdr var))) ; a proper list
              (let ((varnames var)
                    (varnames-sym (i-gensym spec))
                    (list-sym (i-gensym spec))
                    (var-sym (i-gensym spec))
                    name)
                (oset driver variables
                      (cons `(,var-sym ',var) (oref driver variables)))
                (while varnames
                  (setq name (car varnames) varnames (cdr varnames))
                  (oset driver variables
                        (cons name (oref driver variables))))
                `((let ((,varnames-sym ,var-sym) (,list-sym (car ,i-list)))
                    (while ,varnames-sym
                      (set (car ,varnames-sym) (car ,list-sym))
                      (setq ,varnames-sym (cdr ,varnames-sym)
                            ,list-sym (cdr ,list-sym)))
                    (setq ,i-list ,@(list
                                     (if i-iterator `(funcall ,i-iterator ,i-list)
                                       `(cdr ,i-list))))))))
             ((consp var)               ; a (key . value) pair
              (let ((key (car var))
                    (value (cdr var)))
                (oset driver variables
                      (append (list key) (list value)
                              (oref driver variables)))
                `((setq ,key (caar ,i-list) ,value (cdar ,i-list)
                        ,i-list ,@(list
                                   (if i-iterator `(funcall ,i-iterator ,i-list)
                                     `(cdr ,i-list)))))))
             (t                           ; just a single variable
              (oset driver variables
                    (cons var (oref driver variables)))
              `((setq ,var (car ,i-list)
                      ,i-list ,@(list
                                 (if i-iterator `(funcall ,i-iterator ,i-list)
                                   `(cdr ,i-list))))))))
      (oset driver exit-conditions (list i-list)))))

(defmethod i-aggregate-property ((spec i-spec) property &optional extractor)
  (with-slots (drivers) spec
    (let ((ds drivers) result)
      (while ds
        (if extractor
            (setq result
                  (funcall extractor (slot-value (car ds) property) result))
          (push (slot-value (car ds) property) result))
        (setq ds (cdr ds))) result)))

(defmethod i-gensym ((spec i-spec))
  (with-slots (gen-index) spec
    (incf gen-index)
    (intern (concat "--" (number-to-string (1- gen-index))))))

(defmethod i-add-driver ((spec i-spec) driver)
  (with-slots (drivers) spec
    ;; Cannot use add-to-list here
    (unless (member driver drivers)
      (push driver drivers))))

(defmethod i-remove-driver ((spec i-spec) driver)
  (with-slots (drivers) spec
    (setq drivers (remove driver drivers))))

(defun i--parse-repeat (exp spec)
  (let ((sym (i-gensym spec)))
    (with-slots (drivers) spec
      (push 
       (make-instance
        'i-driver
        :variables (list (list sym 0))
        :exit-conditions (list (list '< sym (car exp)))
        :actions (list (list 'incf sym))) drivers))))

(defun i--parse-for (exp spec)
  (let ((driver (make-instance 'i-driver)))
    (funcall (gethash (cadr exp) i-for-handlers) spec driver exp)
    (oset spec drivers (cons driver (oref spec drivers)))))

(defun i--parse-exp (exp spec)
  (pcase exp
    (`(repeat . ,rest)
     (i--parse-repeat rest spec))
    (`(for . ,rest)
     (i--parse-for rest spec))
    (_ (with-slots (body) spec
         (push exp body)))))

(defun i--parse-specs (specs)
  "Parses SPECS and creates an AST represented in an instance of
`i-spec' class."
  (let ((s specs)
        (i (make-instance 'i-spec)))
    (while s
      (i--parse-exp (car s) i)
      (setq s (cdr s))) i))

(defmacro i-iterate (&rest specs)
  (let ((spec (i--parse-specs specs)))
    (with-slots (body result drivers has-exclusive-hash-p) spec
      (let* ((exit-conditions
              (i-aggregate-property spec 'exit-conditions #'append))
             (catch-conditions
              (i-aggregate-property spec 'catch-conditions #'append))
             (variables
              (i-aggregate-property spec 'variables #'append))
             (actions
              (i-aggregate-property spec 'actions #'append))
             (econds
              (cond
               ((cdr exit-conditions)
                (append '(and) (nreverse exit-conditions)))
               (exit-conditions (car exit-conditions))
               (t t)))
             (vars (nreverse variables))
             (body (append actions (nreverse body)))
             (hash-driver (when has-exclusive-hash-p (car drivers))))
        (message "exit-conditions %s, catch-conditions %s, vars %s, actions %s econds"
                 exit-conditions catch-conditions vars actions econds)
        (cond
         (has-exclusive-hash-p
          (let ((key (car (oref hash-driver vars)))
                (value (cadr (oref hash-driver vars)))
                (hash (caddr (oref hash-driver vars)))
                (hash-sym (i-gensym spec)))
            (unless (eq econds t)
              (push '((catch '--maphash)) catch-conditions))
            (append catch-conditions
                    (list
                     `(let* ((,hash-sym hash) ,@(cddr vars))
                        (maphash
                         (lambda (,key ,value)
                           ,@(unless (eq econds t)
                               `(unless ,econds (throw '--maphash nil)))
                           ,@body) ,hash-sym)
                        result)))))
         ((and catch-conditions vars)
          (append catch-conditions
                  (list
                   `(let* (,@vars)
                      (while ,econds ,@body) result))))
         (catch-conditions
          (append catch-conditions
                  (list
                   `(while ,econds ,@body) result)))
         (variables
          `(let* (,@vars)
             (while ,econds ,@body) ,result))
         (t `(progn (while ,econds ,@body) ,result)))))))

(provide 'i-iterate)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; i-iterate.el ends here.
