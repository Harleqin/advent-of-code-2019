(in-package #:aoc-2019)

(defpackage #:aoc-2019/1
  (:use #:cl #:arrows #:aoc-2019))

(in-package #:aoc-2019/1)

(defun aoc1a (&optional (ms (read-integers "1")))
  (loop :for m :in ms
        :sum (mass-fuel m)))

(defun aoc1b (&optional (ms (read-integers "1")))
  (loop :for m :in ms
        :sum (module-fuel m)))

(defun module-fuel (mass)
  (loop :for m := (mass-fuel mass) :then (mass-fuel m)
        :while (plusp m)
        :sum m))

(defun mass-fuel (m)
  (- (floor m 3) 2))
