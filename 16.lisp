(in-package #:aoc-2019)

(defpackage #:aoc-2019/16
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:cl-ppcre
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/16)

(defun aoc16a (&optional (raw-signal (read-signal "16"))
               &aux (signal (parse-signal raw-signal)))
  (map nil #'princ (subseq (fft signal 100) 0 8)))

(defun read-signal (filename)
  (with-open-file (in filename)
    (read-line in)))

(defun parse-signal (s)
  (map 'vector #'digit-char-p s))

(defun fft (signal count)
  (loop :repeat count
        :do (setf signal (fft-phase signal)))
  signal)

(defun fft-phase (signal)
  (map-into (make-array (length signal))
            (lambda (i)
              (->> signal
                   (map 'vector #'*
                        (make-pattern i (length signal)))
                   (reduce #'+)
                   (->* (rem 10))
                   abs))
            (iota (length signal))))

(defun make-pattern (count length)
  (let ((pattern (make-array length))
        (base #(0 1 0 -1)))
    (loop :for p :upfrom -1 :below length
          :for i :upfrom 0
          :unless (minusp p)
            :do (setf (aref pattern p)
                      (aref base (mod (floor i (1+ count)) 4))))
    pattern))
