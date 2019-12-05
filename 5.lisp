(in-package #:aoc-2019)

(defpackage #:aoc-2019/5
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/5)

(defun aoc5a (&optional (program (coerce (read-integers "5") 'vector)))
  (aoc-2019/intcode:intcode program))
