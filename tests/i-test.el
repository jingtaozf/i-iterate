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

;; This is a tests suite for i-iterate package.

;; i-test is NOT part of GNU Emacs.

;;; Versions:
;;
;;    0.0.0 - This is not usable yet.
;;

;;; Usage: Eval buffer and run M-x ert RET t RET

;;; Todo:


;;; Code:

(require 'i-iterate)

;; TODO: This is the case of α-equivalence, which we are not
;; <http://en.wikipedia.org/wiki/Alpha_conversion#Alpha_equivalence>
;; doing properly at the moment. I.e. we should record the
;; environment and the free variables in environment and compare
;; their usage in different functions. Some day I'm smarter,
;; I might do it...
(defun i/test-equals-ignore-gensym (a b)
  "Tests trees A and B for equality, but considers symbols
equal if their names are equal (this allows symbols generated
by `i-gensym' to pass)."
  (or (equal a b)
      (cond
       ((and (null a) (null b)) t)
       ((and (consp a) (consp b))
        (and (i/test-equals-ignore-gensym (car a) (car b))
             (i/test-equals-ignore-gensym (cdr a) (cdr b))))
       ((and (symbolp a) (symbolp b))
        (string= (symbol-name a) (symbol-name b)))
       ((and (atom a) (atom b)) (eql a b))
       (t nil))))

(defun i/test-explainer-equal (a b)
  "Explains why `i/test-equals-ignore-gensym' failed.
This is, basically, only slightly altered `ert--explain-equal'"
  (if (not (i/test-equals-ignore-gensym (type-of a) (type-of b)))
      `(different-types ,a ,b)
    (etypecase a
      (cons
       (let ((a-proper-p (ert--proper-list-p a))
             (b-proper-p (ert--proper-list-p b)))
         (if (not (eql (not a-proper-p) (not b-proper-p)))
             `(one-list-proper-one-improper ,a ,b)
           (if a-proper-p
               (if (not (i/test-equals-ignore-gensym
                         (length a) (length b)))
                   `(proper-lists-of-different-length
                     ,(length a) ,(length b)
                     ,a ,b
                     first-mismatch-at
                     ,(ert--mismatch a b))
                 (loop for i from 0
                       for ai in a
                       for bi in b
                       for xi = (i/test-explainer-equal ai bi)
                       do (when xi (return `(list-elt ,i ,xi)))
                       finally (assert (i/test-equals-ignore-gensym a b) t)))
             (let ((car-x (i/test-explainer-equal (car a) (car b))))
               (if car-x
                   `(car ,car-x)
                 (let ((cdr-x (i/test-explainer-equal (cdr a) (cdr b))))
                   (if cdr-x
                       `(cdr ,cdr-x)
                     (assert (i/test-equals-ignore-gensym a b) t)
                     nil))))))))
      (array (if (not (i/test-equals-ignore-gensym (length a) (length b)))
                 `(arrays-of-different-length
                   ,(length a) ,(length b)
                   ,a ,b
                   ,@(unless (char-table-p a)
                       `(first-mismatch-at
                         ,(ert--mismatch a b))))
               (loop for i from 0
                     for ai across a
                     for bi across b
                     for xi = (i/test-explainer-equal ai bi)
                     do (when xi (return `(array-elt ,i ,xi)))
                     finally (assert (i/test-equals-ignore-gensym a b) t))))
      (atom (if (and (not (i/test-equals-ignore-gensym a b))
                     (not (and (symbolp a) (symbolp b) (string= a b))))
                `(different-atoms ,(ert--explain-format-atom a)
                                  ,(ert--explain-format-atom b))
              nil)))))
(put 'i/test-equals-ignore-gensym
     'ert-explainer 'i/test-explainer-equal)

(defmacro expansion-equals (before-expansion after-expansion)
  "Just a shorthand for comparing the outcome of an expected macroexpansion to
the actual expansion."
  `(should
    (i/test-equals-ignore-gensym
     (macroexpand ',before-expansion)
     ',after-expansion)))

(ert-deftest i-test-repeat ()
  "Tests the expansion of `i-iterate' macro, (repeat *) driver."
  (expansion-equals
   (++ (repeat 100) (message ""))
   (let* ((--0 0))
     (while (< --0 100)
       (incf --0)
       (message "")))))

(ert-deftest i-test-for-in-list ()
  "Tests the expansion of `i-iterate' macro, (for (*) in **) driver."
  (expansion-equals
   (++ (for (a b) in '((1 2) (3 4)))
     (message "a: %s, b: %s" a b))
   (let* ((--0 (quote ((1 2) (3 4)))) (--3 (quote (a b))) a b)
     (while --0
       (let ((--1 --3) (--2 (car --0)))
         (while --1
           (set (car --1) (car --2))
           (setq --1 (cdr --1) --2 (cdr --2)))
         (setq --0 (cdr --0)))
       (message "a: %s, b: %s" a b)))))

(ert-deftest i-test-for-in-symbol ()
  "Tests the expansion of `i-iterate' macro, (for * in **) driver."
  (expansion-equals
   (++ (for i in '(1 2 3)) (message "i: %s" i))
   (let* ((--0 (quote (1 2 3))) i)
     (while --0
       (setq i (car --0) --0 (cdr --0))
       (message "i: %s" i)))))

(ert-deftest i-test-for-in-alist ()
  "Tests the expansion of `i-iterate' macro, (for (* . *) in **) driver."
  (expansion-equals
   (++ (for (a . b) in '((1 . -1) (2 . -2) (3 . -3)))
     (message "a: %s . b: %s" a b))
   (let* ((--0 (quote ((1 . -1) (2 . -2) (3 . -3)))) b a)
     (while --0
       (setq a (caar --0) b (cdar --0) --0 (cdr --0))
       (message "a: %s . b: %s" a b)))))

(ert-deftest i-test-for-from-to ()
  "Tests the expansion of `i-iterate' macro, (for (* . *) in **) driver."
  (expansion-equals
   (++ (for i from 0 to 10) (message "i: %s" i))
   (let* ((i 0))
     (while (<= i 10)
       (message "i: %s" i)
       (incf i)))))

(ert-deftest i-test-for-across-symbol ()
  "Tests the expansion of `i-iterate' macro, (for * across **) driver."
  (expansion-equals
   (i-iterate (for i across [1 2 3 4]) (message "i: %d" i))
   (let* ((--0 [1 2 3 4]) (--1 0) i)
     (while (< --1 (length --0))
       (setq i (aref --0 --1))
       (incf --1)
       (message "i: %d" i)))))

(ert-deftest i-test-for-across-list ()
  "Tests the expansion of `i-iterate' macro, (for (*) across **) driver."
  (let ((test-array [[[1 2 3 4] [5 5 5 5] [1 2 3 4]]
                     [[4 4 4 4] [1 2 3 4] [1 2 3 4]]
                     [[1 2 3 4] [a b c d] [1 2 3 4]]
                     [[1 2 3 4] [1 2 3 4] [i ii iii iv]]]))
    (expansion-equals
     (++ (for (i j k) across test-array)
       (message "i: %s, j: %s, k: %s" i j k)
       (collect (list i j k)))
     (let* (--5 (--0 test-array)
                (--1 0) k j i)
       (while (< --1 (length --0))
         (setq i (aref --0 --1))
         (incf --1)
         (let ((--2 0))
           (while (< --2 (length i))
             (setq j (aref i --2))
             (incf --2)
             (let ((--3 0))
               (while (< --3 (length j))
                 (setq k (aref j --3))
                 (incf --3)
                 (setq --5 (cons (list i j k) --5))
                 (message "i: %s, j: %s, k: %s" i j k))))))
       (nreverse --5)))))

(ert-deftest i-test-for-pairs ()
  "Tests the expansion of `i-iterate' macro, (for (*) pairs **) driver."
  (let ((test-hash (make-hash-table)))
    (dotimes (i 200)
      (puthash i (- i) test-hash))
    (expansion-equals
     (++ (for (a b) pairs test-hash limit 100)
       (collect (cons a b))
       (message "a: %s, b: %s" a b))
     (let* ((--0 0) a b --2)
       (catch (quote --1)
         (maphash 
          (lambda (k v)
            (when (or (> --0 100))
              (throw (quote --1) (nreverse --2)))
            (setq a k b v)
            (setq --2 (cons (cons a b) --2))
            (message "a: %s, b: %s" a b)) test-hash))))))

(ert-deftest i-test-for-pairs-pairs ()
  "Tests the expansion of `i-iterate' macro, (for (*) pairs **) driver
when used in combination with more pairs."
  (let ((test-hash (make-hash-table)))
    (dotimes (i 200)
      (puthash i (- i) test-hash))
    (expansion-equals
     (++ (for (a b) pairs test-hash limit 100)
       (for (c d) pairs test-hash limit 120)
       (collect (list (cons a b) (cons c d)))
       (message "a: %s, b: %s, c: %s, d: %s" a b c d))
     (let* (d c (--3 (let (--2)
                       (maphash
                        (lambda (k v)
                          (setq --2 (cons k --2)))
                        test-hash)
                       (nreverse --2)))
              (--4 0) b a (--0 0) --5)
       (catch (quote --1)
         (maphash
          (lambda (k v)
            (when (or (> --4 120) (> --0 100))
              (throw (quote --1) (nreverse --5)))
            (setq a k b v)
            (setq c (car --3) d (gethash (car --3) test-hash) --3 (cdr --3))
            (incf --4)
            (setq --5 (cons (list (cons a b) (cons c d)) --5))
            (message "a: %s, b: %s, c: %s, d: %s" a b c d))
          test-hash))))))

(ert-deftest i-test-with ()
  "Tests the expansion of `i-iterate' macro, (for (*) pairs **) driver
when used in combination with more pairs."
  (expansion-equals
   (++ (with ((a (let ((x 0)) 
                   (map 'vector #'(lambda (y) (+ y (incf x)))
                        (make-vector 10 0))))
              (b 100) c))
     (for i across a)
     (message "b - i: %s, c: %s" (- b i) c))
   (let* ((--1 0)
          i c (b 100)
          (a (let ((x 0))
               (map (quote vector)
                    (function (lambda (y) (+ y (incf x))))
                    (make-vector 10 0)))))
     (while (< --1 (length a))
       (setq i (aref a --1))
       (incf --1)
       (message "b - i: %s, c: %s" (- b i) c)))))

(ert-deftest i-test-for-on-list ()
  "Tests the expansion of `i-iterate' macro, (for (*) on **) driver
when used in combination with more pairs."
  (expansion-equals
   (++ (for (a b c) on '(1 2 3 4 5))
     (message "a: %s, b: %s, c: %s" a b c))
   (let* ((--0 (quote (1 2 3 4 5))) (--3 (quote (a b c))) (--2 --0) a b c)
     (while --2
       (let ((--1 --3))
         (setq --2 --0)
         (while --1
           (set (car --1) (car --2))
           (setq --1 (cdr --1) --2 (cdr --2)))
         (setq --0 (cdr --0)))
       (message "a: %s, b: %s, c: %s" a b c)))))

(ert-deftest i-test-for-on-alist ()
  "Tests the expansion of `i-iterate' macro, (for (*) on **) driver
when used in combination with more pairs."
  (expansion-equals
   (++ (for (a . b) on '(1 2 3 4 5))
     (message "a: %s, b: %s" a b))
   (let* ((--0 (quote (1 2 3 4 5))) b a)
     (while --0
       (setq a (car --0) b (cdr --0) --0 (cdr --0))
       (message "a: %s, b: %s" a b)))))

(ert-deftest i-test-for-on-symbol ()
  "Tests the expansion of `i-iterate' macro, (for (*) on **) driver
when used in combination with more pairs."
  (expansion-equals
   (++ (for a on '(1 2 3 4 5))
     (message "a: %s" a))
   (let* ((--0 (quote (1 2 3 4 5))) a)
     (while --0
       (setq a --0 --0 (cdr --0))
       (message "a: %s" a)))))

(ert-deftest i-test-for-downfrom-hash ()
  "Tests the expansion of `i-iterate' macro, (for (*) on **) driver
when used in combination with more pairs."
  (expansion-equals
   (++ (for i downfrom 10 to -10 by 2)
     (hash i (* 2 i))
     (message "i: %s" i))
   (let* ((--0 (make-hash-table)) (i 10))
     (while (>= i -10)
       (puthash i (* 2 i) --0)
       (message "i: %s" i)
       (decf i 2)) --0)))

(ert-deftest i-test-for-upfrom-to-output ()
  "Tests the expansion of `i-iterate' macro,
 (for * upfrom ** to ***) in combination with
 (output *) driver."
  (expansion-equals
   (++ (for i upfrom 42 to 54)
     (output 
      (if (oddp i) 
          (print (format "odd: %s" i))
        (print (format "even: %s" i))) into result))
   (let* ((result
           (get-buffer-create
            (generate-new-buffer-name " *string-output*")))
          (i 42))
     (unwind-protect
         (progn
           (while (<= i 54)
             (let ((standard-output result))
               (if (oddp i)
                   (print (format "odd: %s" i))
                 (print (format "even: %s" i))))
             (incf i))
           (with-current-buffer result
             (buffer-string)))
       (kill-buffer result)))))

(ert-deftest i-test-for-words-in ()
  "Tests the expansion of (for * words in **) `i-iterate' macro."
  (expansion-equals
   (++ (for i words in "i-iterate.el")
     (message "i: %s" i))
   (let* (i)
     (while (with-current-buffer "i-iterate.el" (not (eobp)))
       (with-current-buffer "i-iterate.el"
         (setq i (buffer-substring (point) (progn (forward-word) (point)))))
       (message "i: %s" i)))))

(ert-deftest i-test-for-permutations-quickperm ()
  "Tests the expansion of (for * permutations **) `i-iterate' macro."
  (expansion-equals
   (++ (for a permutations [1 2 3 4])
     (message "array: %s" a))
   (let* ((a [1 2 3 4])
          (--4 (length a))
          --0 (--3 (make-vector --4 0))
          (--1 1) --2)
     (while (< --1 --4)
       (if (< (aref --3 --1) --1)
           (progn
             (setq --2 (if (oddp --1) (aref --3 --1) 0) --0 (aref a --2))
             (aset a --2 (aref a --1))
             (aset a --1 --0)
             (message "array: %s" a)
             (aset --3 --1 (1+ (aref --3 --1)))
             (setq --1 1))
         (aset --3 --1 0)
         (incf --1))))))

(ert-deftest i-test-for-permutations-johnson-trotter ()
  "Tests the expansion of (for * permutations ** algo johnson-trotter)
`i-iterate' macro."
  (expansion-equals
   (++ (for a permutations [1 2 3 4] algo johnson-trotter)
     (message "array: %s" a))
   (let* ((a [1 2 3 4])
          (--7 (length a))
          --5 --4 --3 --2 --0
          (--6 (let ((i 0) (m (make-vector --7 nil)))
                 (while (< i --7)
                   (aset m i (cons (quote 1-) i))
                   (incf i))
                 (setcar (aref m 0) nil) m))
          (--1 0))
     (while (some (function car) --6)
       (setq --1 0 --2 nil)
       (while (< --1 --7)
         (destructuring-bind (tested-sign . tested-value)
             (aref --6 --1)
           (when (and tested-sign (or (not --2) (< --2 tested-value)))
             (setq --2 tested-value --3 --1 --4 tested-sign)))
         (incf --1))
       (when --2
         (setq --5 (funcall --4 --3))
         (setq --0 (aref a --3))
         (aset a --3 (aref a --5))
         (aset a --5 --0)
         (setq --0 (aref --6 --3))
         (aset --6 --3 (aref --6 --5))
         (aset --6 --5 --0)
         (when (or (= --5 0)
                   (= --5 (1- --7))
                   (> (cdr (aref --6 (funcall --4 --5))) --2))
           (setcar (aref --6 --5) nil))
         (setq --1 0)
         (while (< --1 --7)
           (setq --5 (cdr (aref --6 --1)))
           (when (> --5 --2)
             (setcar (aref --6 --1)
                     (if (< --1 --3)
                         (quote 1+)
                       (quote 1-))))
           (incf --1))
         (message "array: %s" a))))))

(ert-deftest i-test-for-random-with ()
  "Tests the expansion of (for * random ** to ***)
`i-iterate' macro."
  (expansion-equals
   (++ (with ((s (make-string 90 ?-))))
     (for i random 10 to 100)
     (aset s (- i 10) ?x)
     (message "%s" s))
   (let* ((--7 0) --4 --3 --2 --1
          (--0 (make-vector (ceiling 90 31) 0))
          i (s (make-string 90 45)))
     (while (< --7 90)
       (setq --1 (random 90))
       (let* ((--5 (floor --1 31))
              (--6 (lsh 1 (- --1 (* --5 31)))))
         (if (= (logand (aref --0 --5) --6) 0)
             (aset --0 --5 (logior (aref --0 --5) --6))
           (setq --3 (1- --1) --4 (1+ --1) --2 t)
           (while --2
             (when (> --3 0)
               (setq --5 (floor --3 31)
                     --6 (lsh 1 (- --3 (* --5 31))))
               (if (= (logand (aref --0 --5) --6) 0)
                   (setf --1 --3 --2 nil
                         (aref --0 --5)
                         (logior (aref --0 --5) --6))
                 (decf --3)))
             (when (and --2 (< --4 90))
               (setq --5 (floor --4 31)
                     --6 (lsh 1 (- --4 (* --5 31))))
               (if (= (logand (aref --0 --5) --6) 0)
                   (setf --1 --4 --2 nil
                         (aref --0 --5)
                         (logior (aref --0 --5) --6))
                 (incf --4))))))
       (incf --7)
       (setq i (+ --1 10))
       (aset s (- i 10) 120)
       (message "%s" s)))))

(ert-deftest i-test-for-from-to-by-finally ()
  "Tests the expansion of (for * from ** to *** by ****) in
combination with (finally *) expression of `i-iterate' macro."
  ;; This may seem somewhat controversial, it will print
  ;; 26 instead of intuitively correct 20, bu this is
  ;; consistent with how (loop ...) works too, so
  ;; I'll keep it like this.
  (expansion-equals
   (++ (for i from 1 to 10 by 3)
     (message "i: %s" i)
     (finally (message "i: %s" (* i 2))))
   (let* ((i 1))
     (while (<= i 10)
       (message "i: %s" i)
       (incf i 3))
     (message "i: %s" (* i 2)))))

(ert-deftest i-test-for-on-by-return ()
  "Tests the expansion of (for * on ** by ***) in combination
with (return *) subexpression of `i-iterate' macro."
  (expansion-equals
   (++ (for i on '(1 2 3 4) by #'cddr)
     (when (oddp (car i))
       (return (* (car i) 3))))
   (let* ((--0 (quote (1 2 3 4))) i)
     (catch (quote --1)
       (while --0
         (setq i --0 --0 (cddr --0))
         (if (oddp (car i))
             (throw (quote --1)
                    (* (car i) 3))))))))

(ert-deftest i-test-for-depth-first ()
  "Tests the expansion of (for * depth-first **) 
of `i-iterate' macro."
  (expansion-equals
   (++ (for i depth-first
            '((1 (2 3) ((4) 5) ((((6 7 8))) 9 10 (11)))))
     (message "i: %s" i))
   (let* (i --1 (--0 (quote ((1 (2 3) ((4) 5)
                                ((((6 7 8))) 9 10 (11)))))))
     (while (or --0 --1)
       (cond
        ((null --0)
         (setq --0 (car --1) --1 (cdr --1)))
        ((consp (car --0))
         (setf --1 (cons (cdr --0) --1) --0 (car --0)))
        (t (setq i (car --0))
           (message "i: %s" i)
           (setq --0 (cdr --0))))))))

(ert-deftest i-test-for-breadth-first ()
  "Tests the expansion of (for * breadth-first **) 
of `i-iterate' macro."
  (expansion-equals
   (++ (for i breadth-first
            '((1 (2 3) ((4) 5) ((((6 7 8))) 9 10 (11)))))
     (message "i: %s" i))
   (let* (i (--0 (quote ((1 (2 3) ((4) 5)
                            ((((6 7 8))) 9 10 (11))))))
            (--2 --0) (--1 --0))
     (while --2
       (setq --2 nil)
       (while --1
         (setq i (car --1))
         (cond
          ((and (consp i) (null (cdr i)))
           (setq --2 (cons (car i) --2)))
          ((consp i)
           (setq --2 (cons (car i) --2) --2 (cons (cdr i) --2)))
          (t (message "i: %s" i)))
         (setq --1 (cdr --1)))
       (setq --1 --2)))))

;;; i-test.el ends here.
