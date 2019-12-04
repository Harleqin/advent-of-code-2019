(in-package #:aoc-2019)

(defpackage #:aoc-2019/4
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/4)

(defun aoc4a (start end)
  (loop :for i :from start :below end
        :count (pw-candidate-a-p i)))

(defun pw-candidate-a-p (i)
  (let ((digits (digits i)))
    (and (every #'<= digits (rest digits))
         (some #'= digits (rest digits)))))

(defun digits (n)
  (map 'list
       #'char-code
       (princ-to-string n)))

(defun aoc4b (start end)
  (loop :for i :from start :below end
        :count (pw-candidate-b-p i)))

(defun pw-candidate-b-p (i)
  (let ((digits (digits i)))
    (and (every #'<= digits (rest digits))
         (member 2 (hash-table-values (frequencies digits))))))
