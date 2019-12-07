(in-package #:aoc-2019)

(defpackage #:aoc-2019/7
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:aoc-2019/intcode
        #:arrows
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/7)

(defun aoc7a (&optional (amp (coerce (read-integers "7")
                                     'vector)))
  (let ((outputs ()))
    (map-permutations (lambda (phases)
                        (push (run-amps (copy-seq amp)
                                        phases)
                              outputs))
                      #(0 1 2 3 4))
    (reduce #'max outputs)))

(defun run-amps (program phases)
  (loop :for phase :across phases
        :for input := 0 :then output
        :for output := (amplifier program phase input)
        :finally (return output)))

(defun amplifier (program phase input)
  (let ((*input* (list phase input))
        (*output* nil))
    (intcode program)
    (pop *output*)))
