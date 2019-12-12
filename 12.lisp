(in-package #:aoc-2019)

(defpackage #:aoc-2019/12
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:cl-ppcre
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/12)

(defun aoc12a (&optional
                 (coords (read-coords "12"))
                 (steps 1000))
  (loop :repeat (1+ steps)
        :for moons := (mapcar (lambda (pos)
                                (cons pos (vector 0 0 0)))
                              coords)
          :then new-moons
        :for vs := (loop :for moon :in moons
                         :for (pos . v) := moon
                         :for others := (mapcar #'car (remove moon moons))
                         :collect (vector+ v
                                           (delta-v pos others)))
        :for new-moons := (loop :for pos :in (mapcar #'car moons)
                                :for v :in vs
                                :collect (cons (vector+ pos v) v))
        :do (print moons)
        :finally (return (energy moons))))

(defun read-coords (filename)
  (with-open-file (in filename)
    (loop :for line := (read-line in nil)
          :while line
          :collect (->> (scan-to-strings "<x=(.*),\\s+y=(.*),\\s+z=(.*)>"
                                         line)
                        (nth-value 1)
                        (map 'vector #'parse-integer)))))

(defun delta-v (pos others)
  (reduce #'vector+
          (loop :for other :in others
                :for delta := (map 'vector #'- other pos)
                :collect (map 'vector #'signum delta))))

(defun vector+ (v0 v1)
  (map 'vector #'+ v0 v1))

(defun energy (moons)
  (loop :for (pos . v) :in moons
        :for pot := (reduce #'+ (map 'vector #'abs pos))
        :for kin := (reduce #'+ (map 'vector #'abs v))
        :sum (* pot kin)))

(defun aoc12b (&optional
                 (coords (read-coords "12")))
  (let ((states (make-hash-table :test #'equalp)))
    (loop :for moons := (mapcar (lambda (pos)
                                  (cons pos (vector 0 0 0)))
                                coords)
            :then new-moons
          :for vs := (loop :for moon :in moons
                           :for (pos . v) := moon
                           :for others := (mapcar #'car (remove moon moons))
                           :collect (vector+ v
                                             (delta-v pos others)))
          :for new-moons := (loop :for pos :in (mapcar #'car moons)
                                  :for v :in vs
                                  :collect (cons (vector+ pos v) v))
          :until (gethash moons states)
          :do (setf (gethash moons states) t)
          :count t)))
