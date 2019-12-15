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
        (map (make-hash-table :test #'equalp))
        (open-ends (all-dirs))
        (target nil))
    (loop :for pos := #(0 0) :then next-pos
          :for dir := (move-dir map pos (first open-ends))
          :for try-pos := (map 'vector #'+ pos dir)
          :for status := (progn (chanl:send (first droid)
                                            (eswitch (dir :test #'equalp)
                                              (+n+ 1)
                                              (+s+ 2)
                                              (+e+ 3)
                                              (+w+ 4)))
                                (chanl:recv (second droid)))
          :for next-pos := (if (= status +wall+)
                               pos
                               try-pos)
          :do (ensure-gethash try-pos map status)
              (cond ((= status +wall+)
                     (removef open-ends try-pos :count 1))
                    ((equalp try-pos (first open-ends))
                     (pop open-ends)
                     (dolist (d (all-dirs))
                       (let ((p (map 'vector #'+ try-pos d)))
                         (unless (gethash p map)
                           (push p open-ends))))))
          :when (= status +target+)
            :do (setf target try-pos)
          :until (endp open-ends))
    (length (shortest-path map #(0 0) target))))

(defun move-dir (map from to)
  (or (first (member (map 'vector #'- to from)
                     (all-dirs)
                     :test #'equalp))
      (let ((last-known (if (gethash to map)
                            to
                            (loop :for d :in (all-dirs)
                                  :for p := (map 'vector #'+ to d)
                                  :for s := (gethash p map)
                                    :thereis (and (member s
                                                          (list +open+
                                                                +target+))
                                                  p)))))
        (first (shortest-path map from last-known)))))

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
                        :do (return (trace-path directions neighbour)))
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
                 :for dir := (gethash to directions)
                 :for prev := (some->> dir (map 'vector #'- to))
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
