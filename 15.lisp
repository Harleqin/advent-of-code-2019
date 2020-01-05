(in-package #:aoc-2019)

(defpackage #:aoc-2019/15
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:cl-ppcre
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/15)

(defun aoc15a (&optional (program (coerce (read-integers "15")
                                          'vector)))
  (let ((droid (aoc-2019/intcode:intcode program))
        (map (make-hash-table :test #'equalp)))
    (setf (gethash #(0 0) map) +open+)
    (loop :for level :upfrom 0
          :for (actives pos foundp) := (list (list #(0 0))
                                             #(0 0)
                                             nil)
            :then (expand actives map droid pos)
          :when foundp
            :return level)))

(defun aoc15b (&optional (program (coerce (read-integers "15")
                                          'vector)))
  (let+ ((droid (aoc-2019/intcode:intcode program))
         (map-ht (alist-hash-table (list (cons #(0 0) +open+))
                                   :test #'equalp))
         (oxsys
          (loop :for level :upfrom 0
                :for (actives pos found-oxsys) := (list (list #(0 0))
                                                        #(0 0)
                                                        nil)
                  :then (expand actives map-ht droid pos)
                :for oxsys := (or oxsys found-oxsys)
                :while actives
                :finally (return oxsys)))
         ((&values map min-x min-y)
          (hashtable->matrix map-ht :default-element 0)))
    (loop :for level :upfrom 0
          :for actives := (list (map 'vector #'-
                                     oxsys
                                     (vector min-x min-y)))
            :then (loop :for active :in actives
                        :append (loop :for d :in (all-dirs)
                                      :for n := (map 'vector #'+ active d)
                                      :for s := (screen-ref map n)
                                      :when (= s +open+)
                                        :do (setf (screen-ref map n) +target+)
                                        :and :collect n))
          :while actives
          :finally (return (1- level)))))

(defmacro screen-ref (screen pos)
  `(apply #'aref
          ,screen
          (reverse (coerce ,pos 'list))))

(defun expand (actives map droid pos)
  (loop :with found-target := nil
        :for current-pos := pos :then active
        :for active :in actives
        :do (move-path droid map current-pos active)
        :append (loop :for d :in (all-dirs)
                      :for n := (map 'vector #'+ active d)
                      :for known-status := (gethash n map)
                      :for new-status := (unless known-status
                                           (probe droid d))
                      :when new-status
                        :do (setf (gethash n map) new-status)
                            (when (= new-status +target+)
                              (setf found-target n))
                        :and
                          :when (= new-status +open+)
                            :collect n)
          :into new-actives
        :finally (return (list new-actives active found-target))))

(defun move-path (droid map from to)
  (dolist (dir (shortest-path map from to))
    (let ((status (move droid dir)))
      (assert (= status +open+)
              ()
              "Should be 1 (+open+), but was ~a."
              status))))

(defun probe (droid dir)
  (let ((status (move droid dir)))
    (unless (= status +wall+)
      (move droid (map 'vector #'- dir)))
    status))

(defun move (droid dir)
  (chanl:send (first droid)
              (eswitch (dir :test #'equalp)
                (+n+ 1)
                (+s+ 2)
                (+e+ 3)
                (+w+ 4)))
  (chanl:recv (second droid)))

(defun shortest-path (map from to)
  (let ((loose-ends (make-instance 'unique-heap))
        (lengths (make-hash-table :test #'equalp))
        (directions (make-hash-table :test #'equalp))
        (done (make-hash-table :test #'equalp)))
    (setf (gethash from lengths) 0)
    (flet ((heuristic (f)
             (+ (manhattan-distance f to)
                (gethash f lengths))))
      (heap-upsert loose-ends (heuristic from) from)
      (loop :for current := (heap-pop loose-ends)
            :for next-length := (some-> (gethash current lengths) 1+)
            :while current
            :do (loop :for (dir . neighbour) :in (free-neighbours current map)
                      :do (minf (gethash neighbour lengths ∞) next-length)
                      :unless (gethash neighbour done)
                        :do (let ((betterp (heap-upsert loose-ends
                                                        (heuristic neighbour)
                                                        neighbour)))
                              (when betterp
                                (setf (gethash neighbour directions) dir)))
                      :when (equalp neighbour to)
                        :do (return-from shortest-path 
                              (trace-path directions neighbour)))
                (setf (gethash current done) t)))))

(defun manhattan-distance (a b)
  (loop :for ai :across a
        :for bi :across b
        :sum (abs (- ai bi))))

(defun free-neighbours (x map)
  (loop :for d :in (all-dirs)
        :for n := (map 'vector #'+ x d)
        :when (some-> (gethash n map)
                      (member (list +open+ +target+)))
          :collect (cons d n)))

(defun trace-path (directions to)
  (reverse (loop :for x := to :then prev
                 :for dir := (gethash x directions)
                 :for prev := (some->> dir
                                       (map 'vector #'- x))
                 :while prev
                 :collect dir)))

;; using screen coordinates

(defconstant ∞ most-positive-fixnum)

(define-constant +n+ #(0 -1) :test #'equalp)
(define-constant +e+ #(1 0) :test #'equalp)
(define-constant +s+ #(0 1) :test #'equalp)
(define-constant +w+ #(-1 0) :test #'equalp)

(defmacro all-dirs ()
  '(list +n+ +e+ +s+ +w+))

(defenum
  +wall+
  +open+
  +target+)
