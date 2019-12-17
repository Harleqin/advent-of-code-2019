(in-package #:aoc-2019)

(defpackage #:aoc-2019/17
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:cl-ppcre
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/17)

(defun aoc17a (&optional (program (coerce (read-integers "17")
                                          'vector)))
  (let* ((robot (aoc-2019/intcode:intcode program))
         (view
           (with-output-to-string (v)
             (loop :for output := (chanl:recv (second robot))
                   :until (eq output :end)
                   :do (princ (code-char output) v))))
         (map (read-map view)))
    (alignment map)))

(defun read-map (view)
  (read-matrix (with-input-from-string (v view)
                 (loop :for line := (read-line v nil)
                       :while (some-> line length plusp)
                       :collect line))))

(defun alignment (map)
  (loop :for y :upfrom 1 :below (1- (array-dimension map 0))
        :sum (loop :for x :from 1 :below (1- (array-dimension map 1))
                   :when (char= #\#
                                (aref map y x)
                                (aref map (1- y) x)
                                (aref map (1+ y) x)
                                (aref map y (1- x))
                                (aref map y (1+ x)))
                     :sum (* y x))))
