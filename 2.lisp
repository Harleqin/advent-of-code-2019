(in-package #:aoc-2019)

(defpackage #:aoc-2019/2
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:let-plus))

(in-package #:aoc-2019/2)

(defun aoc2a (&optional (ns (read-integers "2")))
  (setf (nth 1 ns) 12
        (nth 2 ns) 2)
  (aoc-2019/intcode:intcode-single (coerce ns 'vector)))

#+simpl-impl
(defun intcode-l (ns)
  (loop :for (op a b to) :on ns :by #'cddddr
        :do (ecase op
              (1 (setf (nth to ns) (+ (nth a ns) (nth b ns))))
              (2 (setf (nth to ns) (* (nth a ns) (nth b ns))))
              (99 (return-from intcode-l ns)))))

(defun aoc2b (&optional (ns (coerce (read-integers "2") 'vector)))
  (loop :for noun :from 0 :below (length ns)
        :do (loop :for verb :from 0 :below (length ns)
                  :for memory := (copy-array ns)
                  :do (setf (aref memory 1) noun
                            (aref memory 2) verb)
                      (when (= (aoc-2019/intcode:intcode-single memory)
                               19690720)
                        (return-from aoc2b (+ (* 100 noun) verb))))))

