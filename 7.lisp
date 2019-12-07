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
  (max-output amp #'run-amps #(0 1 2 3 4)))

(defun max-output (amp runner possible-phases)
  (let ((outputs ()))
    (map-permutations (lambda (phases)
                        (push (funcall runner
                                       (copy-seq amp)
                                       phases)
                              outputs))
                      possible-phases)
    (reduce #'max outputs)))

(defun run-amps (program phases)
  (loop :for phase :across phases
        :for input := 0 :then output
        :for output := (amplifier (copy-seq program) phase input)
        :finally (return output)))

(defun amplifier (program phase input)
  (destructuring-bind (in out)
      (intcode program :interactivep nil)
    (chanl:send in phase)
    (chanl:send in input)
    (chanl:recv out)))

(defun aoc7b (&optional (amp (coerce (read-integers "7")
                                     'vector)))
  (max-output amp #'run-amps-feedback #(5 6 7 8 9)))

(defun run-amps-feedback (program phases)
  (let ((amps (loop :repeat 5
                    :collect (intcode (copy-seq program)
                                      :interactivep nil))))
    (loop :for (in out) :in amps
          :for phase :across phases
          :do (chanl:send in phase))
    (chanl:send (first (first amps)) 0)
    (loop :for sender :in (apply #'circular-list amps)
          :for receiver :in (rest (apply #'circular-list amps))
          :for signal := (chanl:recv (second sender))
          :for output := (if (numberp signal) signal output)
          :until (eq signal :end)
          :do (chanl:send (first receiver) signal)
          :finally (return output))))
