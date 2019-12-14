(in-package #:aoc-2019)

(defpackage #:aoc-2019/14
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:cl-ppcre
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/14)

(defun aoc14a (&optional (raw-reactions (read-file-into-string "14"))
               &aux (reactions (parse-reactions raw-reactions)))
  (ore-need reactions 1))

(defun ore-need (reactions fuel)
  (let ((back-steps (make-hash-table :test #'equal)))
    (loop :for ((count product) ingredients) :in reactions
          :do (setf (gethash product back-steps)
                    (list count ingredients)))
    (loop :for state := (list (make-instance 'bucket
                                             :thing :fuel
                                             :need fuel))
            :then (let* ((bucket (find-if (lambda (bucket)
                                            (and (not (eq (bucket-thing bucket) :ore))
                                                 (plusp (bucket-need bucket))))
                                          state))
                         (product (bucket-thing bucket))
                         (count (bucket-need bucket)))
                    (unreact back-steps
                             state
                             product
                             count))
              :thereis (only-ore state))))

(defun parse-reactions (string)
  (with-input-from-string (in string)
    (loop :for line := (read-line in nil)
          :while line
          :collect (parse-reaction line))))

(defun parse-reaction (line)
  (let+ (((left right)
          (split "\\s*=>\\s*" line))
         (ingredients (->> (split "\\s*,\\s*" left)
                           (mapcar #'parse-part)))
         (product (parse-part right)))
    (list product ingredients)))

(defun parse-part (s)
  (destructuring-bind (count x) (split "\\s+" s)
    (list (parse-integer count)
          (intern x '#:keyword))))

(defun unreact (back-steps state product need)
  (let+ (((ecount ings) (gethash product back-steps))
         ((&values factor excess) (ceiling need ecount))
         (expansion (loop :for (icount thing) :in ings
                          :collect (make-instance 'bucket
                                                  :thing thing
                                                  :need (* icount factor)))))
    (merge-states (substitute (make-instance 'bucket
                                             :thing product
                                             :need excess)
                              product
                              state
                              :key #'bucket-thing
                              :count 1)
                  expansion)))

(defun merge-states (a b)
  (sort (loop :for key :in (union (mapcar #'bucket-thing a)
                                  (mapcar #'bucket-thing b))
              :for ax := (find key a :key #'bucket-thing)
              :for bx := (find key b :key #'bucket-thing)
              :collect (make-instance 'bucket
                                      :thing key
                                      :need (+ (or (some-> ax bucket-need) 0)
                                               (or (some-> bx bucket-need) 0))))
        #'string<
        :key (lambda (bucket)
               (string (bucket-thing bucket)))))

(defun only-ore (state)
  (when (every (lambda (bucket)
                 (or (eq (bucket-thing bucket) :ore)
                     (not (plusp (bucket-need bucket)))))
               state)
    (bucket-need (find :ore state
                       :key #'bucket-thing))))

(defclass bucket ()
  ((thing :initarg :thing :reader bucket-thing)
   (need :initarg :need :reader bucket-need)))

(defmethod print-object ((b bucket) stream)
  (print-unreadable-object (b stream :type t)
    (mapc (rcurry #'princ stream)
          (list (bucket-thing b)
                " need: " (bucket-need b)))))

(defun aoc14b (&optional
                 (raw-reactions (read-file-into-string "14"))
                 (ore (expt 10 12))
               &aux
                 (reactions (parse-reactions raw-reactions)))
  (loop :for min := (/ ore (ore-need reactions 1))
          :then (if enough-ore-p fuel min)
        :for max := (* 2 min)
          :then (if enough-ore-p max fuel)
        :for fuel := (round (/ (+ min max) 2))
        :for enough-ore-p := (<= (ore-need reactions fuel) ore)
        :until (= (- max min) 1)
        :when enough-ore-p
          :maximize fuel))
