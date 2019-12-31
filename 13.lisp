(in-package #:aoc-2019)

(defpackage #:aoc-2019/13
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:cl-ppcre
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/13)

(defun aoc13a (&optional (program (coerce (read-integers "13")
                                          'vector)))
  (let+ ((arcade (aoc-2019/intcode:intcode program))
         (cable (make-hash-table :test #'equalp))
         ((max-x max-y)
          (loop :for x := (chanl:recv (second arcade))
                :for y := (if (eq x :end)
                              x
                              (chanl:recv (second arcade)))
                :for tile := (if (eq x :end)
                                 x
                                 (chanl:recv (second arcade)))
                :until (eq x :end)
                :do (setf (gethash (vector x y) cable)
                          (case tile
                            (0 #\space)
                            (1 #\#)
                            (2 #\*)
                            (3 #\-)
                            (4 #\o)))
                :maximize x :into max-x
                :maximize y :into max-y
                :finally (return (list (1+ max-x) (1+ max-y)))))
         (screen (make-array (list max-y max-x)
                             :initial-element #\space)))
    (maphash (lambda (k v)
               (setf (aref screen (aref k 1) (aref k 0))
                     v))
             cable)
    (let ((block-count (count #\* (hash-table-values cable) :test #'char=)))
      (print-matrix screen)
      (values block-count max-x max-y))))

(defun aoc13b (&optional (program (coerce (read-integers "13")
                                          'vector)))
  (let+ (((&values &ign max-x max-y)
          (let ((*standard-output* (make-broadcast-stream)))
            (aoc13a (copy-seq program))))
         ((&values &ign) (setf (aref program 0) 2))
         (screen (make-array (list max-y max-x)
                             :initial-element #\space))
         (buffer (make-array 3))
         (pointers (circular-list 0 1 2))
         (score 0)
         (paddle-x 0)
         (ball-x 0)
         ((&flet arcade-in ()
            (format t "> ~a <~%" score)
            (print-matrix screen)
            (when (zerop (count #\* (array-flat-view screen)))
              (break))
            (signum (- ball-x paddle-x))))
         ((&flet arcade-out (n)
            (let ((p (pop pointers)))
              (setf (aref buffer p) n)
              (when (= p 2)
                (if (= (aref buffer 0) -1)
                    (setf score (aref buffer 2))
                    (let+ ((#(x y tile-id) buffer))
                      (setf (screen-ref screen (vector x y))
                            (tile-char tile-id))
                      (case tile-id
                        (3 (setf paddle-x x))
                        (4 (setf ball-x x))))))))))
    (format t "~a quarters~%" (aref program 0))
    (aoc-2019/intcode:intcode-core program
                                   #'arcade-in
                                   #'arcade-out)
    (princ "Game over")
    (terpri)
    (print-matrix screen)
    score))

(defun parse-joystick (line)
  (case (char line 0)
    ;; neo-layout
    ((#\i #\n) -1)
    ((#\e #\t) 1)
    (t 0)))

(defun tile-char (tile-id)
  (case tile-id
    (0 #\space)
    (1 #\#)
    (2 #\*)
    (3 #\-)
    (4 #\o)))

(defmacro screen-ref (screen pos)
  `(apply #'aref
          ,screen
          (reverse (coerce ,pos 'list))))
