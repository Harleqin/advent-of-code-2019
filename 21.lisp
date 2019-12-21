(in-package #:aoc-2019)

(defpackage #:aoc-2019/21
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:cl-ppcre
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/21)

(defun aoc21a ()
  (springdroid "NOT A J"
               "NOT B T"
               "OR T J"
               "NOT C T"
               "OR T J"
               "AND D J"
               "WALK"))

(defun springdroid (&rest instructions)
  (let* ((program (coerce (read-integers "21")
                                          'vector))
         (droid (aoc-2019/intcode:intcode program)))
    (print-prompt droid)
    (dolist (instruction instructions)
      (ascii-in droid instruction))
    (print-ascii-output droid)))

(defun print-prompt (droid)
  (loop :for c := (chanl:recv (second droid))
        :do (princ (code-char c))
        :while (/= c 10)))

(defun ascii-in (droid &rest lines)
  (dolist (line lines)
    (print line)
    (loop :for c :across line
          :do (chanl:send (first droid) (char-code c)))
    (chanl:send (first droid) 10)))

(defun print-ascii-output (droid)
  (loop :for o := (chanl:recv (second droid))
        :until (eq o :end)
        :thereis (when (> o 127) o)
        :do (princ (code-char o))))

(defun aoc21b ()
  (springdroid "NOT A J"
               "NOT B T"
               "OR T J"
               "NOT C T"
               "OR T J"
               "AND D J"
               "NOT E T"
               "NOT T T"
               "OR H T"
               "AND T J"
               "RUN"))
