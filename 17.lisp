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
                                          'vector)))
  (setf (aref program 0) 2)
  (let ((instructions "A,A,B,C,A,C,A,B,C,B
R,12,L,8,R,6
R,12,L,6,R,6,R,8,R,6
L,8,R,8,R,6,R,12
n
")
        (ip 0))
    (flet ((input ()
             (prog1 (char-code (char instructions ip))
               (incf ip)))
           (output (n)
             (cond ((eq :end n) nil)
                   ((< n 128)
                    (princ (code-char n)))
                   (t
                    (format t "~%-> ~a~%" n)))))
      (aoc-2019/intcode:intcode-core program
                                     #'input
                                     #'output))))



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
