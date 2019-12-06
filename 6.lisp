(in-package #:aoc-2019)

(defpackage #:aoc-2019/6
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/6)

(defun aoc6a (&optional (orbits (read-orbits "6")))
  (let ((deps (orbit-child-tree orbits)))
    (loop :for parents := (list "COM") :then children
          :for children := (mapcan (lambda (parent)
                                     (gethash parent deps))
                                   parents)
          :for level :upfrom 1
          :while children
          :sum (* level (length children)))))

(defun orbit-child-tree (orbits)
  (let ((deps (make-hash-table :test #'equal)))
    (loop :for (parent child) :in orbits
          :do (ensure-gethash parent deps ())
              (push child (gethash parent deps)))
    deps))

(defun aoc6b (&optional (orbits (read-orbits "6")))
  (let* ((deps (orbit-parent-tree orbits))
         (you-chain (orbit-chain deps "YOU"))
         (san-chain (orbit-chain deps "SAN"))
         (common (mismatch you-chain san-chain :test #'string=)))
    (- (+ (length you-chain) (length san-chain))
       (* 2 common)
       2)))

(defun orbit-parent-tree (orbits)
  (let ((deps (make-hash-table :test #'equal)))
    (loop :for (parent child) :in orbits
          :do (setf (gethash child deps) parent))
    deps))

(defun orbit-chain (tree obj)
  (reverse (loop :for o := obj :then (gethash o tree)
                 :while (string/= o "COM")
                 :collect o)))

(defun read-orbits (filename)
  (with-open-file (in filename)
    (loop :for line := (read-line in nil)
          :while line
          :collect (split-sequence #\) line))))

