(in-package #:aoc-2019)

(defpackage #:aoc-2019/11
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/11)

(defun aoc11a (&optional (program (coerce (read-integers "11")
                                          'vector)))
  (let ((hull (make-hash-table :test #'equalp))
        (robot (aoc-2019/intcode:intcode program)))
    (loop :for pos := #(0 0) :then next-pos
          :for dir := #(0 1) :then next-dir
          :for current-colour := (gethash pos hull 0)
          :for &ign0 := (chanl:send (first robot) current-colour)
          :for new-colour := (chanl:recv (second robot))
          :for &ign1 := (when (eq new-colour :end) (return))
          :for turn := (chanl:recv (second robot))
          :for next-dir := (turn dir turn)
          :for next-pos := (map 'vector #'+ pos next-dir)
          :do (setf (gethash pos hull) new-colour))
    (hash-table-count hull)))

(defparameter *directions* #(#(1 0) #(0 1) #(-1 0) #(0 -1)))

(defun turn (from turn &aux (by (ecase turn (0 1) (1 -1))))
  (aref *directions* (mod (+ (position from *directions*
                                       :test #'equalp)
                             by)
                          4)))
