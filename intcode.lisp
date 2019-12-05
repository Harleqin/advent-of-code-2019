(in-package #:aoc-2019)

(defpackage #:aoc-2019/intcode
  (:use #:cl
        #:alexandria
        #:arrows)
  (:export #:intcode))

(in-package #:aoc-2019/intcode)

(defparameter *ops* (make-hash-table))

(defvar *ip+* nil)

(defmacro define-op (opcode posargs &body body)
  (with-gensyms (ip
                 parameter-modes
                 memvar)
    `(setf (gethash ,opcode *ops*)
           (lambda (,memvar ,ip ,parameter-modes)
             ,(when (zerop (length posargs))
                `(declare (ignore ,memvar ,parameter-modes)))
             (let ((*ip+* nil))
               (symbol-macrolet
                   (,@(loop :for posarg :in posargs
                            :for i :from 0
                            :collect
                            `(,posarg
                              (-> (vector (when (array-in-bounds-p ,memvar
                                                                   (aref ,memvar (+ ,ip 1 ,i)))
                                            (make-array 1
                                                        :displaced-to ,memvar
                                                        :displaced-index-offset
                                                        (aref ,memvar (+ ,ip 1 ,i))))
                                          (vector (aref ,memvar (+ ,ip 1 ,i))))
                                  (aref (aref ,parameter-modes ,i))
                                  (aref 0)))))
                 ,@body
                 (or *ip+* (+ ,ip 1 ,(length posargs)))))))))

#+example
(setf (gethash 1 *ops*)
      (lambda (memory ip parameter-modes)
        (let ((*ip* nil))
          (symbol-macrolet
              ((a (-> (vector (when (array-in-bounds-p memory (aref memory (+ ip 1 0)))
                                (make-array 1
                                            :displaced-to memory
                                            :displaced-index-offset (aref memory (+ ip 1 0))))
                              (vector (aref memory (+ ip 1 0))))
                      (aref (aref parameter-modes 0))
                      (aref 0)))
               (b (-> (vector (make-array 1
                                          :displaced-to memory
                                          :displaced-index-offset (aref memory (+ ip 1 1)))
                              (vector (aref memory (+ ip 1 1))))
                      (aref (aref parameter-modes 1))
                      (aref 0)))
               (to (-> (vector (make-array 1
                                           :displaced-to memory
                                           :displaced-index-offset (aref memory (+ ip 1 2)))
                               (vector (aref memory (+ ip 1 2))))
                       (aref (aref parameter-modes 2))
                       (aref 0))))
            (setf to (+ a b))
            4))))

(define-op 1 (a b to)
  (setf to (+ a b)))

(define-op 2 (a b to)
  (setf to (* a b)))

(define-op 3 (to)
  (princ "Input: ")
  (finish-output)
  (setf to
        (parse-integer (read-line) :junk-allowed t)))

(define-op 4 (from)
  (format t "Output: ~a~%" from))

(define-op 5 (pred jump)
  (unless (zerop pred)
    (setf *ip+* jump)))

(define-op 6 (pred jump)
  (when (zerop pred)
    (setf *ip+* jump)))

(define-op 7 (a b to)
  (setf to (if (< a b) 1 0)))

(define-op 8 (a b to)
  (setf to (if (= a b) 1 0)))

(define-op 99 ()
  (setf *ip+* 'end))

(defun intcode (memory)
  (loop :for ip := 0
          :then next-ip
        :for (opcode parameter-modes) := (parse-instruction (aref memory ip))
        :for next-ip := (funcall (gethash opcode *ops*) memory ip parameter-modes)
        :when (eq next-ip 'end)
          :do (return memory)))

(defun parse-instruction (n)
  (multiple-value-bind (modes opcode) (floor n 100)
    (list opcode
          (loop :with ps := (make-array 10
                                        :fill-pointer 0
                                        :adjustable t)
                :for (ms m) := (multiple-value-list (floor modes 10))
                  :then (multiple-value-list (floor ms 10))
                :while (or (plusp ms) (plusp m))
                :do (vector-push-extend m ps)
                :finally (return ps)))))
