(in-package #:aoc-2019)

(defpackage #:aoc-2019/3
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/3)

(defun aoc3a (&optional (wires (read-wires "3")))
  (let* ((path-a (trace-wire (first wires)))
         (path-b (trace-wire (second wires)))
         (xs (2d-intersection path-a path-b))
         (ds (mapcar #'manhattan xs)))
    (reduce #'min ds)))

(defun aoc3b (&optional (wires (read-wires "3")))
  (let* ((path-a (trace-wire (first wires)))
         (path-b (trace-wire (second wires)))
         (xs (2d-intersection path-a path-b))
         (x-steps (make-hash-table :test #'equalp)))
    (loop :for path :in (list path-a path-b)
          :for wire :in '(0 1)
          :do (loop :for pos :in path
                    :for steps :upfrom 1
                    :when (and (member pos xs :test #'equalp)
                               (zerop (aref (ensure-gethash pos
                                                            x-steps
                                                            (vector 0 0))
                                            wire)))
                      :do (setf (aref (gethash pos x-steps) wire)
                                steps)))
    (values (loop :for s :being :the :hash-values :of x-steps
                  :minimize (reduce #'+ s))
            x-steps)))

(defun 2d-intersection (xs ys)
  (let ((axis (make-hash-table)))
    (dolist (x xs)
      (push (aref x 1) (gethash (aref x 0) axis ())))
    (values (loop :for y :in ys
                  :when (member (aref y 1)
                                (gethash (aref y 0) axis ()))
                    :collect y)
            axis)))

(defun trace-wire (segments)
  (loop :for pos := #(0 0)
          :then (add-segment pos dir count)
        :for (dir . count) :in segments
        :append (trace-segment pos dir count)))

(defun trace-segment (pos dir count)
  (loop :for i :from 1 :to count
        :collect (add-segment pos dir i)))

(defun add-segment (pos dir count)
  (map 'vector
       #'+
       pos
       (case dir
         (#\R (vector count 0))
         (#\L (vector (- count) 0))
         (#\U (vector 0 count))
         (#\D (vector 0 (- count))))))

(defun manhattan (x)
  (+ (abs (aref x 0)) (abs (aref x 1))))

(defun read-wires (filename)
  (with-open-file (in filename)
    (loop :repeat 2
          :collect (parse-wire (read-line in)))))

(defun parse-wire (line)
  (->> line
       (split-sequence #\,)
       (mapcar #'parse-segment)))

(defun parse-segment (s)
  (cons (char s 0)
        (parse-integer s :start 1)))
