(in-package #:aoc-2019)

(defpackage #:aoc-2019/19
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:cl-ppcre
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/19)

(defun aoc19a (&optional (program (coerce (read-integers "19")
                                          'vector)))
  (loop :for y :below 50
        :sum (loop :for x :below 50
                   :for pullp := (drone-test program x y)
                   :count pullp)))

(defun drone-test (program x y)
  (let ((drone (aoc-2019/intcode:intcode program)))
    (chanl:send (first drone) x)
    (chanl:send (first drone) y)
    (= 1 (chanl:recv (second drone)))))

(defun aoc19b (&optional (program (coerce (read-integers "19")
                                          'vector)))
  (loop :for (x y) := (find-beam program 100 0 -1 +1)
          :then (find-beam program (1+ x) y -1 +1)
        :when (drone-test program (- x 99) (+ y 99))
          :do (return (+ (* 10000 (- x 99))
                         y))))

(defun find-beam (program x y dx dy)
  (loop :for (xn yn) := (list x y)
          :then (list (+ xn dx) (+ yn dy))
        :until (drone-test program xn yn)
        :finally (return (list xn yn))))

