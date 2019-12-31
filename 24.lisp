(in-package #:aoc-2019)

(defpackage #:aoc-2019/24
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:cl-ppcre
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/24)

(defun aoc24a ()
  (loop :with ht := (make-hash-table)
        :for (state h w) := (read-state (read-file-into-string "24"))
          :then (evolve state h w)
        :when (gethash state ht)
          :do (return state)
        :do (setf (gethash state ht) state)))

(defun read-state (string)
  (let+ ((map (read-map string))
         ((height width) (array-dimensions map)))
    (list (loop :for y :below height
                :sum (loop :for x :below width
                           :when (char= (aref map y x) #\#)
                             :sum (ash 1 (+ (* y width) x))))
          height
          width)))

(defun evolve (state h w)
  (list (loop :for i :below (* h w)
              :for neighbour-count := (+ (if (minusp (- i w))
                                             0
                                             (ldb (byte 1 (- i w)) state))
                                         (if (zerop (mod i w))
                                             0
                                             (ldb (byte 1 (1- i)) state))
                                         (if (zerop (mod (1+ i) w))
                                             0
                                             (ldb (byte 1 (1+ i)) state))
                                         (if (> (+ i w) (* h w))
                                             0
                                             (ldb (byte 1 (+ i w)) state)))
              :when (or (and (logbitp i state)
                             (= neighbour-count 1))
                        (and (not (logbitp i state))
                             (< 0 neighbour-count 3)))
                :sum (ash 1 i))
        h
        w))
