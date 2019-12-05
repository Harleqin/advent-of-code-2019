(in-package #:aoc-2019)

(defvar *ops* (make-array 100 :initial-element #'identity))

(defvar *op-lengths* (make-array 100 :initial-element 0))

(defmacro define-op (opcode posargs memvar &body body)
  (with-gensyms (instruction-pointer)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (aref *ops* ,opcode)
             (lambda (,memvar ,instruction-pointer)
               (let ,(loop :for posarg :in posargs
                           :for i :from 1
                           :collect `(,posarg (aref ,memvar (+ ,instruction-pointer ,i))))
                 ,@body))
             (aref *op-lengths* ,opcode)
             (1+ (length ',posargs))))))

(define-op 1 (a b to) memory
  (setf (aref memory to)
        (+ (aref memory a)
           (aref memory b))))

(define-op 2 (a b to) memory
  (setf (aref memory to)
        (* (aref memory a)
           (aref memory b))))

(define-op 3 (to) memory
  (princ "Input: ")
  (finish-output)
  (setf (aref memory to)
        (parse-integer (read-line) :junk-allowed t)))

(define-op 4 (from) memory
  (format t "Output: ~a~%" (aref memory from)))

(defun intcode (memory)
  (loop :for ip := 0
          :then (+ ip (aref *op-lengths* opcode))
        :for opcode := (aref memory ip)
        :when (= opcode 99)
          :do (return memory)
        :do (funcall (aref *ops* opcode) memory ip)))
