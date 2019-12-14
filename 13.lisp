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
  (let+ (((&values &ign max-x max-y) (aoc13a (copy-seq program)))
         (&ign (setf (aref program 0) 2))
         (arcade (aoc-2019/intcode:intcode program))
         (screen (make-array (list max-y max-x)
                             :initial-element #\space))
         (score 0))
    (loop :repeat 3 :do (chanl:send (first arcade) 1))
    (loop :for x := (let ((x (chanl:recv (second arcade))))
                      (if (eq x :end)
                          (loop-finish)
                          x))
          :for y := (chanl:recv (second arcade))
          :for pos := (vector x y)
          :for tile-id := (chanl:recv (second arcade))
          :for tile := (if (eql x -1)
                           (setf score tile-id)
                           (setf (aref screen y x)
                                 (case tile-id
                                   (0 #\space)
                                   (1 #\#)
                                   (2 #\*)
                                   (3 #\-)
                                   (4 #\o))))
          :for (ball-prev ball) := (if (eql tile #\o)
                                       (list ball pos)
                                       (list ball-prev ball))
          :for input := (when (eql tile #\-)
                          (print-matrix screen)
                          (parse-integer (read-line)))
          :when input
            :do (chanl:send (first arcade) input))
    score))
