(in-package #:aoc-2019)

(defpackage #:aoc-2019/8
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/8)

(defun aoc8a (&optional (raw-image (read-digits "8")))
  (let ((best-0count nil)
        (best-result nil))
    (loop :for layer :in (layers raw-image)
          :for 0count := (count 0 layer)
          :when (or (null best-0count)
                    (< 0count best-0count))
            :do (setf best-0count 0count
                      best-result (* (count 1 layer)
                                     (count 2 layer))))
    best-result))

(defun read-digits (filename)
  (with-open-file (in filename)
    (map 'vector #'digit-char-p (read-line in))))

(defun layers (image)
  (loop :for l :from 0 :below (length image) :by (* 25 6)
        :collect (subseq image l (+ l (* 25 6)))))

(defun aoc8b (&optional (raw-image (read-digits "8")))
  (let ((image (make-array '(6 25))))
    (loop :for layer :in (reverse (layers raw-image))
          :do (loop :for i :below (* 25 6)
                    :for p :across layer
                    :unless (= p 2)
                      :do (setf (row-major-aref image i) p)))
    (print-image image)))

(defun print-image (image)
  (dotimes (y 6)
    (dotimes (x 25)
      (princ (case (aref image y x)
               (0 #\ )
               (1 #\#))))
    (terpri)))
