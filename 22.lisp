(in-package #:aoc-2019)

(defpackage #:aoc-2019/22
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:cl-ppcre
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/22)

(defstruct (stack (:constructor make-stack (length &key zero delta)))
  length
  (zero 0)
  (delta 1))

(defun aoc22a ()
  (card-position 2019
                 (shuffle-cards (make-stack 10007)
                                (read-file-into-string "22"))))

(defun card-position (card stack)
  (loop :for n :from (stack-zero stack) :by (stack-delta stack)
        :for i :upfrom 0
        :when (= n card)
          :do (return i)))

(defun nth-card (index stack)
  (+ (stack-zero stack)
     (* index (stack-delta stack))))

(defun shuffle-cards (stack text)
  (reduce #'shuffle-step
          (parse-steps text)
          :initial-value stack))

(defun parse-steps (text)
  (mapcar #'parse-step (string-lines text)))

(defun parse-step (line)
  (cond ((string= line "deal into new stack")
         (list :deal-into-new-stack))
        ((every #'char= "cut " line)
         (list :cut (parse-integer line :start (length "cut "))))
        ((every #'char= "deal with increment " line)
         (list :deal-with-increment
               (parse-integer line
                              :start (length "deal with increment "))))
        (t (error "unknown step: ~s" line))))

(defun shuffle-step (stack step)
  (ccase (first step)
    (:deal-into-new-stack
     (make-stack (stack-length stack)
                 :zero (nth-card (1- (stack-length stack)) stack)
                 :delta (- (stack-delta stack))))
    (:cut (let* ((n (second step))
                 (i (if (minusp n)
                        (+ (length stack) n)
                        n)))
            (concatenate 'vector
                         (subseq stack i)
                         (subseq stack 0 i))))
    (:deal-with-increment
     (let* ((n (second step))
            (l (length stack))
            (new (make-array l)))
       (loop :for i := 0 :then (mod (+ i n) l)
             :for card :across stack
             :do (setf (aref new i) card))
       new))))

