(in-package #:aoc-2019)

(defpackage #:aoc-2019/intcode
  (:use #:cl
        #:alexandria
        #:arrows)
  (:export #:intcode
           #:intcode-core
           #:intcode-single))

(in-package #:aoc-2019/intcode)

(defparameter *ops* (make-hash-table))

(defvar *ip+* nil)

(defvar *rb*)

(defvar *interactivep* nil)

(defmacro define-op (opcode posargs (in out) &body body)
  (with-gensyms (ip
                 rb
                 parameter-modes
                 memvar)
    `(setf (gethash ,opcode *ops*)
           (lambda (,memvar ,ip ,rb ,parameter-modes ,in ,out)
             (declare (ignorable ,in ,out)
                      ,@(when (zerop (length posargs))
                          `((ignore ,memvar ,parameter-modes))))
             (let ((*ip+* nil)
                   (*rb* ,rb))
               (symbol-macrolet
                   (,@(loop :for posarg :in posargs
                            :for i :from 0
                            :collect
                            `(,posarg
                              (-> (vector (-> (+ ,ip 1 ,i)
                                              (ensure-gethash ,memvar (list 0))
                                              first
                                              (ensure-gethash ,memvar (list 0)))
                                          (-> (+ ,ip 1 ,i)
                                              (ensure-gethash ,memvar (list 0)))
                                          (-> (+ ,ip 1 ,i)
                                              (ensure-gethash ,memvar (list 0))
                                              first
                                              (+ *rb*)
                                              (ensure-gethash ,memvar (list 0))))
                                  (aref (aref ,parameter-modes ,i))
                                  first))))
                 ,@body
                 (list (or *ip+* (+ ,ip 1 ,(length posargs)))
                       *rb*)))))))

(define-op 1 (a b to) (in out)
  (setf to (+ a b)))

(define-op 2 (a b to) (in out)
  (setf to (* a b)))

(define-op 3 (to) (in out)
  (setf to (funcall in)))

(define-op 4 (from) (in out)
  (funcall out from))

(define-op 5 (pred jump) (in out)
  (unless (zerop pred)
    (setf *ip+* jump)))

(define-op 6 (pred jump) (in out)
  (when (zerop pred)
    (setf *ip+* jump)))

(define-op 7 (a b to) (in out)
  (setf to (if (< a b) 1 0)))

(define-op 8 (a b to) (in out)
  (setf to (if (= a b) 1 0)))

(define-op 9 (from) (in out)
  (incf *rb* from))

(define-op 99 () (in out)
  (funcall out :end)
  (setf *ip+* 'end))

(defun intcode (program)
  (let ((input-ch (make-instance 'chanl:bounded-channel
                                 :size 2))
        (output-ch (make-instance 'chanl:bounded-channel
                                  :size 2)))
    (chanl:pexec ()
      (intcode-core program
                    (lambda ()
                      (chanl:recv input-ch))
                    (lambda (n)
                      (chanl:send output-ch n))))
    (list input-ch output-ch)))

(defun intcode-core (program input-fn output-fn)
  "Runs the given intcode program.  INPUT-FN is a function of zero arguments
that returns a single integer.  OUTPUT-FN is a function of one argument that
returns nothing."
  (loop :with memory := (vector->hashtable program)
        :for ip := 0
          :then next-ip
        :for relative-base := 0
          :then next-rb
        :for (opcode parameter-modes)
          := (parse-instruction (first (gethash ip memory)))
        :for (next-ip next-rb) := (funcall (gethash opcode *ops*)
                                           memory
                                           ip
                                           relative-base
                                           parameter-modes
                                           input-fn
                                           output-fn)
        :until (eq next-ip 'end)
        :finally (return (gethash 0 memory))))

(defun vector->hashtable (v)
  (let ((ht (make-hash-table)))
    (dotimes (i (length v) ht)
      (setf (gethash i ht) (list (aref v i))))))

(defun intcode-single (program)
  (intcode-core program
                (lambda ()
                  (princ "Input: ")
                  (finish-output)
                  (parse-integer (read-line) :junk-allowed t))
                (lambda (n)
                  (format t "Output: ~a~%" n))))

(defun parse-instruction (n)
  (multiple-value-bind (modes opcode) (floor n 100)
    (list opcode
          (loop :with ps := (make-array 10
                                        :fill-pointer 0
                                        :adjustable t
                                        :initial-element 0)
                :for (ms m) := (multiple-value-list (floor modes 10))
                  :then (multiple-value-list (floor ms 10))
                :while (or (plusp ms) (plusp m))
                :do (vector-push-extend m ps)
                :finally (return ps)))))
