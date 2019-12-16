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
                                   (rem (abs sum) 10))))
    output))

(defun aoc16b (&optional (raw-signal (read-signal "16"))
               &aux (whole-signal (repeat 10000 (parse-signal raw-signal))))
  (let* ((offset (parse-integer (subseq raw-signal 0 7)))
         ;; The pattern is always 0 below the currently calculated index.
         ;; Starting at the middle of the signal, it is always 1 above that.  If
         ;; the offset is higher than the middle, we can just sum from the end
         ;; and ignore everytihng before the offset.
         (signal (reverse (subseq whole-signal offset))))
    (assert (>= offset (/ (length whole-signal) 2)))
    (loop :repeat 100
          :do (loop :for i :below (length signal)
                    :for x :across signal
                    :sum x :into sum
                    :do (setf (aref signal i) (rem (abs sum) 10)))
              (princ #\.))
    (map nil #'princ (subseq (reverse signal) 0 8))))
