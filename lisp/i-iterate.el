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
;; Below are few examples of macroexpansions
;; (macroexpand '(i-iterate (repeat 100) (message "hellow there!")))
;; (let* ((--0 0))
;;   (while (< --0 100)
;;     (incf --0)
;;     (message "hellow there!")) nil)

;; (macroexpand
;;  '(i-iterate (repeat 100)
;;              (repeat 43)
;;              (message "hellow there!")))
;; (let* ((--0 0) (--1 0))
;;   (while (and (<= --0 100) (<= --1 43))
;;     (incf --0) (incf --1) (message "hellow there!")) nil)

;; (macroexpand
;;  '(i-iterate (for i from 1 to 42 by 6)
;;              (message "hellow there!")))
;; (let* ((i 1) (--0 6) (--1 42))
;;   (while (<= i --1)
;;     (incf i --0) (message "hellow there!")) nil)

;; (macroexpand
;;  '(i-iterate (for i downfrom 1 to -8 by 2)
;;              (message "hellow there!")))
;; (let* ((i 1) (--0 2) (--1 -8))
;;   (while (>= i --1)
;;     (decf i --0)
;;     (message "hellow there!")) nil)

;; (format "%S" (macroexpand '(i-iterate (for blah in '(1 2 3 4) by #'cddr) (message "blah: %s" blah))))
;; (let* ((--1 (function cddr)) (--0 (quote (1 2 3 4))) blah)
;;   (while --0
;;     (setq blah (car --0) --0 (funcall --1 --0))
;;     (message "blah: %s" blah)) nil)

;; (format "%S" (macroexpand '(i-iterate (for (blah . blerh) in '((1 . 2) (3 . 4) (5 . 6)) by #'cddr)
;;                                       (message "blah: %s blerh %s" blah blerh))))
;; (let* ((--1 (function cddr)) (--0 (quote ((1 . 2) (3 . 4) (5 . 6)))) blerh blah)
;;   (while --0
;;     (setq blah (caar --0) 
;;           blerh (cdar --0)
;;           --0 (funcall --1 --0))
;;     (message "blah: %s blerh %s" blah blerh)) nil)

;; (format "%S" (macroexpand '(i-iterate (for (a b c) in '((1 2 3) (a b c) (i ii iii)) by #'cddr)
;;                                       (message "a: %s b: %s c: %s" a b c))))
;; (let* ((--1 (function cddr)) (--0 (quote ((1 2 3) (a b c) (i ii iii)))) (--4 (quote (a b c))) a b c)
;;   (while --0
;;     (let ((--2 --4) (--3 (car --0)))
;;       (while --2
;;         (set (car --2) (car --3))
;;         (setq --2 (cdr --2) --3 (cdr --3)))
;;       (setq --0 (funcall --1 --0)))
;;     (message "a: %s b: %s c: %s" a b c)) nil)

;; (macroexpand '(i-iterate (for i across [1 2 3 4]) (message "i: %d" i)))
;; (let* ((--0 [1 2 3 4]) (--1 0) i)
;;   (while (< --1 (length --0))
;;     (setq i (aref --0 --1))
;;     (incf --1)
;;     (message "i: %d" i)) nil)

;; (format "%S"
;;         (macroexpand
;;          '(i-iterate (for (i j k) across 
;;                           [[[1 2 3 4] [5 5 5 5] [1 2 3 4]]
;;                            [[4 4 4 4] [1 2 3 4] [1 2 3 4]]
;;                            [[1 2 3 4] [a b c d] [1 2 3 4]]
;;                            [[1 2 3 4] [1 2 3 4] [i ii iii iv]]])
;;                      (message "i: %s, j: %s, k: %s" i j k))))
;; (let* ((--0 [[[1 2 3 4] [5 5 5 5] [1 2 3 4]]
;;              [[4 4 4 4] [1 2 3 4] [1 2 3 4]]
;;              [[1 2 3 4] [a b c d] [1 2 3 4]]
;;              [[1 2 3 4] [1 2 3 4] [i ii iii iv]]])
;;        (--1 0) k j i)
;;   (while (< --1 (length --0))
;;     (setq i (aref --0 --1))
;;     (incf --1)
;;     (let ((--2 0))
;;       (while (< --2 (length i))
;;         (setq j (aref i --2))
;;         (incf --2)
;;         (let ((--3 0))
;;           (while (< --3 (length j))
;;             (setq k (aref j --3))
;;             (incf --3)
;;             (message "i: %s, j: %s, k: %s" i j k)))))) nil)

;; (format "%S"
;;         (macroexpand
;;          '(i-iterate (for (i j k) across 
;;                           [[[1 2 3 4] [5 5 5 5] [1 2 3 4]]
;;                            [[4 4 4 4] [1 2 3 4] [1 2 3 4]]
;;                            [[1 2 3 4] [a b c d] [1 2 3 4]]
;;                            [[1 2 3 4] [1 2 3 4] [i ii iii iv]]] 
;;                           by (1+ (lambda (x) (+ 2 x)) 1+))
;;                      (message "i: %s, j: %s, k: %s" i j k))))
;; (let* ((--2 1+)
;;        (--3 (lambda (x) (+ 2 x)))
;;        (--4 1+)
;;        (--0 [[[1 2 3 4] [5 5 5 5] [1 2 3 4]]
;;              [[4 4 4 4] [1 2 3 4] [1 2 3 4]]
;;              [[1 2 3 4] [a b c d] [1 2 3 4]]
;;              [[1 2 3 4] [1 2 3 4] [i ii iii iv]]])
;;        (--1 0) k j i)
;;   (while (< --1 (length --0))
;;     (setq i (aref --0 --1))
;;     (setq --1 (funcall --4 --1))
;;     (let ((--5 0))
;;       (while (< --5 (length i))
;;         (setq j (aref i --5))
;;         (setq --5 (funcall --3 --5))
;;         (let ((--6 0))
;;           (while (< --6 (length j))
;;             (setq k (aref j --6))
;;             (setq --6 (funcall --2 --6))
;;             (message "i: %s, j: %s, k: %s" i j k)))))) nil)


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
    :type (or list atom)
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
slot is non-nil, then `i-iterate' will generate the `maphash'.")
   (has-body-insertion-p
    :initform nil
    :type symbol
    :documentation "Non-nil if the body must be inserted in a special place, rather
than appended after all forms and before the result form."))
  :documentation "This class contains a specification of the
expansion of the `i-iterate' macro")

(defvar i-for-handlers nil
  "The table of all handlers for (for ...) driver. Use `i-add-for-handler' if you
need to add your own driver")
(unless i-for-handlers (setq i-for-handlers (make-hash-table)))

(defvar i-expanders '((collect . i--parse-collect))
  "The expanders used in the nested forms in the `i-iterate' macro")

(defvar i-spec-stack nil
  "This variable is bound by various expanders when they need to receive the
reference to the `i-spec' instance assigned to this expansion, but it is not
possible to pass it with the arguments")

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
  (destructuring-bind (var verb begin &optional target limit how iterator (op '<=))
      exp
    ;; TODO: need to check for constant expressions in iterator and limit
    ;; to possibly avoid generating extra vairables.
    (oset driver variables (list (list var begin)))
    (let ((act (if (eql verb 'downfrom) 'decf 'incf))
          (sym (when how (i-gensym spec))))
      (if sym 
          (progn
            (oset driver variables
                  (cons (list sym iterator) (oref driver variables)))
            (oset driver actions `((,act ,var ,sym))))
        (oset driver actions `((,act ,var)))))
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

(i-add-for-handler across (spec driver exp)
  (destructuring-bind (var verb iterated-array &optional how iterator)
      exp
    (let* ((i-array (i-gensym spec))
           (i-pos (i-gensym spec))
           iter-sym
           (i-iterator
            (cond
             ((null iterator)
              `(incf ,i-pos))
             ((symbolp iterator)
              (setq iter-sym (i-gensym spec))
              (oset driver variables `((,iter-sym ,iterator)))
              `(setq ,i-pos (funcall ,iter-sym ,i-pos)))
             (t                         ; iterator must be a list
                                        ; of functions
              (let ((it iterator))
                (while it
                  (setq iter-sym (cons (i-gensym spec) iter-sym))
                  (oset driver variables
                        (cons `(,@(list (car iter-sym)) ,@(list (car it)))
                              (oref driver variables)))
                  (setq it (cdr it))) iter-sym)))))
      (oset driver variables
            (append (if (symbolp var) (list var) var)
                    `((,i-pos 0) (,i-array ,iterated-array))
                    (oref driver variables)))
      (if (symbolp var)
          (oset driver actions
                `((setq ,var (aref ,i-array ,i-pos)) ,i-iterator))
        (let ((dimensions (reverse (cons i-array var)))
              (positions
               (nreverse
                (cons i-pos
                      (mapcar (lambda (x) (i-gensym spec)) var))))
              (iterators
               (reverse (or iter-sym (make-list (length var) i-iterator))))
              let-group pos-pre pos-post result)
          (oset spec has-body-insertion-p t)
          (while (cdr dimensions)
            (setq pos-pre (cadr positions)
                  pos-post (car positions)
                  let-group
                  (if let-group
                      `((setq ,@(list (car dimensions))
                              (aref ,@(list (cadr dimensions)) ,pos-pre))
                        ,@(if iter-sym
                              `((setq ,pos-pre
                                      (funcall ,@(list (car iterators)) ,pos-pre)))
                            `((incf ,pos-pre)))
                        (let ((,pos-post 0))
                          (while (< ,pos-post (length ,@(list (car dimensions))))
                            ,@(when let-group let-group))))
                    `((setq ,@(list (car dimensions))
                            (aref ,@(list (cadr dimensions)) ,pos-pre))
                      ,@(if iter-sym
                            `((setq ,pos-pre
                                    (funcall ,@(list (car iterators)) ,pos-pre)))
                          `((incf ,pos-pre)))
                      --i-body--)))
            (setq dimensions (cdr dimensions)
                  iterators (cdr iterators)
                  positions (cdr positions)))
          (oset driver actions let-group)))
      (oset driver exit-conditions
            `((< ,i-pos (length ,i-array)))))))

(i-add-for-handler in (spec driver exp)
  (destructuring-bind (var verb iterated-list how iterator)
      exp
    ;; TODO: need to check for constant expressions in iterator and iterated-list
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

(defun i--parse-collect (exp &optional spec parent-exp)
  (let ((driver (make-instance 'i-driver))
        (spec (or spec  i-spec-stack))
        ;; This looks wrong, we've lost something underway
        (exp (if (consp exp) exp (list exp))))
    (destructuring-bind (form &optional into location)
        exp
      (let ((location (or location (i-gensym spec))))
        (message "collect expansion")
        (oset driver variables `(,location))
        (oset driver actions
              `((setq ,location (cons ,form ,location))))
        (oset spec result location)))
    (oset spec drivers (cons driver (oref spec drivers)))))

(defun i--expand-in-environment (exp &optional env)
  (let ((env (or env macroexpand-all-environment)))
    (macrolet
        ((collect (e)))
      ;; Need to check here for duplicates, so we don't add same
      ;; handlers twice
      (message "environment %s" (append i-expanders env))
      (macroexpand-all exp (append i-expanders env)))))

(defun i--parse-exp (exp spec)
  (pcase exp
    (`(repeat . ,rest)
     (i--parse-repeat rest spec))
    (`(for . ,rest)
     (i--parse-for rest spec))
    (`(collect . ,rest)
     (i--parse-collect rest spec))
    (_ (with-slots (body) spec
         (let ((i-spec-stack spec))
           (push (i--expand-in-environment exp) body))))))

(defun i--parse-specs (specs)
  "Parses SPECS and creates an AST represented in an instance of
`i-spec' class."
  (let ((s specs)
        (i (make-instance 'i-spec)))
    (while s
      (i--parse-exp (car s) i)
      (setq s (cdr s))) i))

(defun i--normalize-body (body)
  (cond
   ((null body) nil)
   ((and (consp body) (cdr body))
    (append '(progn) body))
   ((consp body) (car body))
   (t body)))

(defmacro i-iterate (&rest specs)
  (let ((spec (i--parse-specs specs)))
    (with-slots (body result drivers has-exclusive-hash-p
                      has-body-insertion-p) spec
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
             (body-all (append actions (reverse body)))
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
                           ,@body-all) ,hash-sym)
                        result)))))
         ((and catch-conditions vars)
          (append catch-conditions
                  (list
                   (if has-body-insertion-p
                       (subst (i--normalize-body body) '--i-body--
                              `(let* (,@vars)
                                 (while ,econds ,@actions) result))
                     `(let* (,@vars)
                        (while ,econds ,@body-all) result)))))
         (catch-conditions
          (append catch-conditions
                  (list
                   (if has-body-insertion-p
                       (subst (i--normalize-body body) '--i-body--
                              `(while ,econds ,@actions))
                     `(while ,econds ,@body-all)) result)))
         (variables
          (if has-body-insertion-p
              (subst (i--normalize-body body) '--i-body--
                     `(let* (,@vars)
                        (while ,econds ,@actions) ,result))
            `(let* (,@vars)
               (while ,econds ,@body-all) ,result)))
         (t
          (if has-body-insertion-p
              (subst (i--normalize-body body)
                     '--i-body--
                     `(progn (while ,econds ,@actions) ,result))
            `(progn (while ,econds ,@body-all) ,result))))))))

(provide 'i-iterate)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; i-iterate.el ends here.
