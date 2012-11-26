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

;;; Usage:
;; Below are few examples of macroexpansions

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

;;; Todo:


;;; Code:

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
  "Explains why `i/test-equals-ignore-gensym' failed."
  ;; TODO: Write our own explanation, this will trigger when
  ;; necessary, but will not always display the correct message.
  (ert--explain-equal-rec a b))
(put 'i/test-equals-ignore-gensym
     'ert-explainer 'i/test-explainer-equal)

(ert-deftest i-test-repreat ()
  "Tests the expansion of i-iterate macro, repeat driver."
  (require 'i-iterate)
  (should
   (i/test-equals-ignore-gensym
    (macroexpand '(++ (repeat 100) (message "")))
    '(let* ((--0 0))
       (while (< --0 100)
         (incf --0)
         (message "")) nil))))
