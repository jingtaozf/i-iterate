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

(ert-deftest i-test-repeat ()
  "Tests the expansion of i-iterate macro, (repeat *) driver."
  (require 'i-iterate)
  (should
   (i/test-equals-ignore-gensym
    (macroexpand '(++ (repeat 100) (message "")))
    '(let* ((--0 0))
       (while (< --0 100)
         (incf --0)
         (message "")) nil))))

(ert-deftest i-test-for-in-list ()
  "Tests the expansion of i-iterate macro, (for (*) in **) driver."
  (require 'i-iterate)
  (should
   (i/test-equals-ignore-gensym
    (macroexpand
     '(++ (for (a b) in '((1 2) (3 4)))
        (message "a: %s, b: %s" a b)))
    '(let* ((--0 (quote ((1 2) (3 4)))) (--3 (quote (a b))) a b)
       (while --0
         (let ((--1 --3) (--2 (car --0)))
           (while --1
             (set (car --1) (car --2))
             (setq --1 (cdr --1) --2 (cdr --2)))
           (setq --0 (cdr --0)))
         (message "a: %s, b: %s" a b)) nil))))

(ert-deftest i-test-for-in-symbol ()
  "Tests the expansion of i-iterate macro, (for * in **) driver."
  (require 'i-iterate)
  (should
   (i/test-equals-ignore-gensym
    (macroexpand '(++ (for i in '(1 2 3)) (message "i: %s" i)))
    '(let* ((--0 (quote (1 2 3))) i)
       (while --0
         (setq i (car --0) --0 (cdr --0))
         (message "i: %s" i)) nil))))

(ert-deftest i-test-for-in-alist ()
  "Tests the expansion of i-iterate macro, (for (* . *) in **) driver."
  (require 'i-iterate)
  (should
   (i/test-equals-ignore-gensym
    (macroexpand '(++ (for (a . b) in '((1 . -1) (2 . -2) (3 . -3)))
                    (message "a: %s . b: %s" a b)))
    '(let* ((--0 (quote ((1 . -1) (2 . -2) (3 . -3)))) b a)
       (while --0
         (setq a (caar --0) b (cdar --0) --0 (cdr --0))
         (message "a: %s . b: %s" a b)) nil))))

(ert-deftest i-test-for-from-to ()
  "Tests the expansion of i-iterate macro, (for (* . *) in **) driver."
  (require 'i-iterate)
  (should
   (i/test-equals-ignore-gensym
    (macroexpand '(++ (for i from 0 to 10) (message "i: %s" i)))
    '(let* ((i 0))
       (while (<= i 10)
         (message "i: %s" i)
         (incf i)) nil))))

(ert-deftest i-test-for-across-symbol ()
  "Tests the expansion of i-iterate macro, (for * across **) driver."
  (require 'i-iterate)
  (should
   (i/test-equals-ignore-gensym
    (macroexpand '(i-iterate (for i across [1 2 3 4]) (message "i: %d" i)))
    '(let* ((--0 [1 2 3 4]) (--1 0) i)
       (while (< --1 (length --0))
         (setq i (aref --0 --1))
         (incf --1)
         (message "i: %d" i)) nil))))

(ert-deftest i-test-for-across-list ()
  "Tests the expansion of i-iterate macro, (for (*) across **) driver."
  (require 'i-iterate)
  (let ((test-array [[[1 2 3 4] [5 5 5 5] [1 2 3 4]]
                     [[4 4 4 4] [1 2 3 4] [1 2 3 4]]
                     [[1 2 3 4] [a b c d] [1 2 3 4]]
                     [[1 2 3 4] [1 2 3 4] [i ii iii iv]]]))
    (should
     (i/test-equals-ignore-gensym
      (macroexpand
       '(++ (for (i j k) across test-array)
          (message "i: %s, j: %s, k: %s" i j k)
          (collect (list i j k))))
      '(let* (--5 (--0 test-array)
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
         (nreverse --5))))))

(ert-deftest i-test-for-pairs ()
  "Tests the expansion of i-iterate macro, (for (*) pairs **) driver."
  (require 'i-iterate)
  (let ((test-hash (make-hash-table)))
    (dotimes (i 200)
      (puthash i (- i) test-hash))
    (should
     (i/test-equals-ignore-gensym
      (macroexpand
       '(++ 
          (for (a b) pairs test-hash limit 100)
          (collect (cons a b))
          (message "a: %s, b: %s" a b)))
      '(let* ((--0 0) a b --2)
         (catch (quote --1)
           (maphash
            (lambda (k v)
              (when (or (> --0 100))
                (throw (quote --1) nil))
              (setq a k b v)
              (setq --2 (cons (cons a b) --2))
              (message "a: %s, b: %s" a b))
            test-hash))
         (nreverse --2))))))

(ert-deftest i-test-for-pairs-pairs ()
  "Tests the expansion of i-iterate macro, (for (*) pairs **) driver
when used in combination with more pairs."
  (require 'i-iterate)
  (let ((test-hash (make-hash-table)))
    (dotimes (i 200)
      (puthash i (- i) test-hash))
    (should
     (i/test-equals-ignore-gensym
      (macroexpand
       '(++ 
          (for (a b) pairs test-hash limit 100)
          (for (c d) pairs test-hash limit 120)
          (collect (list (cons a b) (cons c d)))
          (message "a: %s, b: %s, c: %s, d: %s" a b c d)))
      '(let* (d c (--3 (let (--2)
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
                (throw (quote --1) nil))
              (setq a k b v)
              (setq c (car --3) d (gethash (car --3) test-hash) --3 (cdr --3))
              (incf --4)
              (setq --5 (cons (list (cons a b) (cons c d)) --5))
              (message "a: %s, b: %s, c: %s, d: %s" a b c d))
            test-hash))
         (nreverse --5))))))

(ert-deftest i-test-with ()
  "Tests the expansion of i-iterate macro, (for (*) pairs **) driver
when used in combination with more pairs."
  (require 'i-iterate)
  (should
   (i/test-equals-ignore-gensym
    (macroexpand
     '(++
        (with ((a (let ((x 0)) 
                    (map 'vector #'(lambda (y) (+ y (incf x)))
                         (make-vector 10 0))))
               (b 100) c))
        (for i across a)
        (message "b - i: %s, c: %s" (- b i) c)))
    '(let* ((--1 0)
            i c (b 100)
            (a (let ((x 0))
                 (map (quote vector)
                      (function (lambda (y) (+ y (incf x))))
                      (make-vector 10 0)))))
       (while (< --1 (length a))
         (setq i (aref a --1))
         (incf --1)
         (message "b - i: %s, c: %s" (- b i) c)) nil))))

(ert-deftest i-test-for-on-list ()
  "Tests the expansion of i-iterate macro, (for (*) on **) driver
when used in combination with more pairs."
  (require 'i-iterate)
  (should
   (i/test-equals-ignore-gensym
    (macroexpand
     '(++ (for (a b c) on '(1 2 3 4 5))
        (message "a: %s, b: %s, c: %s" a b c)))
    '(let* ((--0 (quote (1 2 3 4 5))) (--3 (quote (a b c))) (--2 --0) a b c)
       (while --2
         (let ((--1 --3))
           (setq --2 --0)
           (while --1
             (set (car --1) (car --2))
             (setq --1 (cdr --1) --2 (cdr --2)))
           (setq --0 (cdr --0)))
         (message "a: %s, b: %s, c: %s" a b c)) nil))))

(ert-deftest i-test-for-on-alist ()
  "Tests the expansion of i-iterate macro, (for (*) on **) driver
when used in combination with more pairs."
  (require 'i-iterate)
  (should
   (i/test-equals-ignore-gensym
    (macroexpand '(++ (for (a . b) on '(1 2 3 4 5))
                    (message "a: %s, b: %s" a b)))
    '(let* ((--0 (quote (1 2 3 4 5))) b a)
       (while --0
         (setq a (car --0) b (cdr --0) --0 (cdr --0))
         (message "a: %s, b: %s" a b)) nil))))

(ert-deftest i-test-for-on-symbol ()
  "Tests the expansion of i-iterate macro, (for (*) on **) driver
when used in combination with more pairs."
  (require 'i-iterate)
  (should
   (i/test-equals-ignore-gensym
    (macroexpand '(++ (for a on '(1 2 3 4 5))
                    (message "a: %s" a)))
    '(let* ((--0 (quote (1 2 3 4 5))) a)
       (while --0
         (setq a --0 --0 (cdr --0))
         (message "a: %s" a)) nil))))

(ert-deftest i-test-for-downfrom-hash ()
  "Tests the expansion of i-iterate macro, (for (*) on **) driver
when used in combination with more pairs."
  (require 'i-iterate)
  (should
   (i/test-equals-ignore-gensym
    (macroexpand '(++ (for i downfrom 10 to -10 by 2)
                    (hash i (* 2 i))
                    (message "i: %s" i)))
    '(let* ((--0 (make-hash-table)) (i 10))
       (while (>= i -10)
         (puthash i (* 2 i) --0)
         (message "i: %s" i)
         (decf i 2)) --0))))

;;; I-test.el ends here.
