(in-package #:aoc-2019)

(defpackage #:aoc-2019/9
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/9)

(defun aoc9a (&optional (program (coerce (read-integers "9")
                                         'vector)))
  (aoc-2019/intcode:intcode-single program))
