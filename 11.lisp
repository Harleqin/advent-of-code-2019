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
    (hash-table-count (run-robot robot hull))))

(defun run-robot (robot hull)
  (loop :for pos := #(0 0) :then next-pos
        :for dir := #(0 -1) :then next-dir
        :for current-colour := (gethash pos hull 0)
        :for &ign0 := (chanl:send (first robot) current-colour)
        :for new-colour := (chanl:recv (second robot))
        :for &ign1 := (when (eq new-colour :end) (return))
        :for turn := (chanl:recv (second robot))
        :for next-dir := (turn dir turn)
        :for next-pos := (map 'vector #'+ pos next-dir)
        :do (setf (gethash pos hull) new-colour))
  hull)

(defparameter *directions* #(#(1 0) #(0 1) #(-1 0) #(0 -1)))

(defun turn (from turn &aux (by (ecase turn (0 -1) (1 1))))
  (aref *directions* (mod (+ (position from *directions*
                                       :test #'equalp)
                             by)
                          4)))

(defun aoc11b (&optional (program (coerce (read-integers "11")
                                          'vector)))
  (let ((hull (make-hash-table :test #'equalp))
        (robot (aoc-2019/intcode:intcode program)))
    (setf (gethash #(0 0) hull) 1)
    (run-robot robot hull)
    (let+ ((all-positions (hash-table-keys hull))
           ((min-x max-x min-y max-y) (extrema all-positions))
           (real-hull (make-array (list (- max-y min-y)
                                        (- max-x min-x))
                                  :initial-element 0)))
      (maphash (lambda (pos colour)
                 (setf (aref real-hull (aref pos 1) (aref pos 0))
                       colour))
               hull)
      (print-matrix real-hull '((0 . #\ ) (1 . #\#))))))

(defun extrema (positions)
  (loop :for pos :in positions
        :minimize (aref pos 0) :into min-x
        :maximize (aref pos 0) :into max-x
        :minimize (aref pos 1) :into min-y
        :maximize (aref pos 1) :into max-y
        :finally (return (list min-x (1+ max-x) min-y (1+ max-y)))))
