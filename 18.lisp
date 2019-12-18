(in-package #:aoc-2019)

(defpackage #:aoc-2019/18
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:cl-ppcre
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/18)

(defun aoc18a (&optional (raw-map (read-file-into-string "18"))
               &aux
                 (map (read-map raw-map))
                 (key-count (count-if #'lower-case-p (array-flat-view map))))
  ;; A cpos is a complex position: a cons of the actual position vector and the
  ;; key set represented as a bit field (integer).  We can then just use
  ;; Dujkstra.
  (loop :with done := (make-hash-table :test #'equalp)
        :for dist :upfrom 0
        :for actives := (list (cons (2d-pos #\@ map) 0))
          :then (mapcan (lambda (cpos)
                          (->> (free-neighbours cpos map)
                               (remove-if (lambda (cp)
                                            (gethash cp done)))))
                        actives)
        :do (dolist (a actives)
              (setf (gethash a done) dist))
        :until (loop :for (&ign . key-set) :in actives
                     :thereis (= key-set (1- (expt 2 key-count))))
        :finally (return dist)))

(defun free-neighbours (cpos map)
  (loop :for dir :in all-dirs
        :for n := (map 'vector #'+ (car cpos) dir)
        :for c := (when (array-in-bounds-p map (aref n 1) (aref n 0))
                    (map-ref map n))
        :when (and c
                   (char/= c #\#)
                   (door-not-closed-p c (cdr cpos)))
          :collect (cons n (collect-keys c (cdr cpos)))))

(defun collect-keys (maybe-key key-set)
  (if (lower-case-p maybe-key)
      (logior key-set (ash 1 (- (char-code maybe-key)
                                (char-code #\a))))
      key-set))

(defun door-not-closed-p (maybe-door key-set)
  (or (not (upper-case-p maybe-door))
      (logbitp (- (char-code maybe-door)
                  (char-code #\A))
               key-set)))

;; Screen coordinates
(defparameter all-dirs (list #(1 0)
                             #(0 1)
                             #(-1 0)
                             #(0 -1)))
