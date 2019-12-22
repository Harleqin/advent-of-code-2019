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

(defun aoc22a ()
  (position 2019
            (shuffle-cards (coerce (iota 10007) 'vector)
                           (read-file-into-string "22"))))

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
    (:deal-into-new-stack (reverse stack))
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

(defun inverse-step (index length step)
  (ccase (first step)
    (:deal-into-new-stack (- length index 1))
    (:cut (let* ((n (second step))
                 (i (if (minusp n)
                        (+ length n)
                        n)))
            (mod (+ index i) length)))
    (:deal-with-increment
     (let* ((n (second step))
            (s (mod length n))
            (r (mod (* s index -1) n))
            (x (floor index n)))
       (+ (* r (floor length n))
          x
          (if (plusp r) s 0))))))

(defun aoc22b (&optional
                 (length 119315717514047)
                 (isteps (reverse (parse-steps (read-file-into-string "22")))))
  (inverse-steps length isteps 2020))

(defun inverse-steps (length isteps x)
  (reduce (lambda (i step)
            (inverse-step i length step))
          isteps
          :initial-value x))
