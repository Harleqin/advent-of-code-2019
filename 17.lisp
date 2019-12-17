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
    (values map
            (alignment map))))

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

(defun aoc17b (&optional (program (coerce (read-integers "17")
                                          'vector))
               &aux (map (aoc17a program)))
  (setf (aref program 0) 2)
  (let ((path (trace-path map)))
    path))

(defun trace-path (map)
  (loop :for pos := (2d-pos-if (lambda (c)
                                 (member c '(#\^ \> #\< #\v)))
                               map)
          :then next-pos
        :for dir := (ecase (map-ref map pos)
                      (#\^ #(0 -1))
                      (#\> #(1 0))
                      (#\v #(0 1))
                      (#\< #(-1 0)))
          :then next-dir
        :for (next-pos next-dir moves) := (find-path map pos dir)
        :while next-pos
        :append moves))

(defun find-path (map pos dir)
  (loop :for (moves . d) :in (list (cons '(1) dir)
                                  (cons '(#\R 1) (turn-right/screen dir))
                                  (cons '(#\L 1) (turn-left/screen dir)))
        :for p := (map 'vector #'+ pos d)
        :when (and (array-in-bounds-p map (aref p 1) (aref p 0))
                   (char= #\# (map-ref map p)))
          :do (return (list p d moves))))

(defun turn-left/screen (dir)
  (v*m dir #2a((0 -1)
               (1 0))))

(defun turn-right/screen (dir)
  (v*m dir #2a((0 1)
               (-1 0))))

(defun v*m (vector matrix)
  (destructuring-bind (h w) (array-dimensions matrix)
    (assert (= h (length vector)))
    (let ((r (make-array w)))
      (loop :for i :below w
            :do (setf (aref r i)
                      (loop :for y :below h
                            :sum (* (aref vector y)
                                    (aref matrix y i)))))
      r)))
