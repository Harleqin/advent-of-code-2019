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
  (let ((output (make-array (length signal)))
        (pattern #(0 1 0 -1)))
    (loop :for o :upfrom 0 :below (length signal)
          :for count :upfrom 1
          :do (loop :for s :across signal
                    :for i :upfrom 0
                    :for p := (aref pattern
                                    (mod (floor (1+ i) count)
                                         4))
                    :sum (* p s) :into sum
                    :finally (setf (aref output o)
                                   (abs (rem sum 10)))))
    output))

