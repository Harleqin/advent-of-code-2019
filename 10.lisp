(in-package #:aoc-2019)

(defpackage #:aoc-2019/10
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/10)

(defun aoc10a (&optional
                 (raw-map (read-asteroids "10"))
                 debugp
               &aux
                 (map (parse-asteroids raw-map))
                 (asteroids (asteroids map)))
  (let ((best-count 0)
        (best-asteroid nil))
    (values (loop :for asteroid :in asteroids
                  :for count := (loop :for other :in asteroids
                                      :unless (eq asteroid other)
                                        :count (los-p map asteroid other))
                  :do (setf (aref map (aref asteroid 1) (aref asteroid 0))
                            count)
                      (when (> count best-count)
                        (setf best-count count
                              best-asteroid asteroid))
                  :maximize count)
            best-asteroid
            (when debugp map))))

(defun asteroids (map)
  (loop :for y :below (array-dimension map 0)
        :nconc (loop :for x :below (array-dimension map 1)
                     :when (aref map y x)
                       :collect (vector x y))))

(defun los-p (map pos-a pos-b)
  (let+ ((#(x0 y0) pos-a)
         (#(x1 y1) pos-b)
         (dx (- x1 x0))
         (dy (- y1 y0))
         (gcd (gcd dx dy))
         (step-x (/ dx gcd))
         (step-y (/ dy gcd)))
    (loop :for x := (+ x0 step-x) :then (+ x step-x)
          :for y := (+ y0 step-y) :then (+ y step-y)
          :until (and (= x x1) (= y y1))
          :never (aref map y x))))

(defun read-asteroids (filename)
  (with-open-file (in filename)
    (loop :for line := (read-line in nil)
          :while line
          :collect line)))

(defun parse-asteroids (lines)
  (let ((map (make-array (list (length lines)
                               (length (first lines))))))
    (loop :for y :from 0
          :for line :in lines
          :do (loop :for x :from 0
                    :for content :across line
                    :do (setf (aref map y x)
                              (char= content #\#))))
    map))

(defun aoc10b (&optional
                 (raw-map (read-asteroids "10"))
                 debugp
               &aux
                 (map (parse-asteroids raw-map))
                 (asteroids (asteroids map)))
  (let ((station (nth-value 1 (aoc10a raw-map)))
        (angles (make-hash-table)))
    (dolist (asteroid asteroids)
      (unless (equalp asteroid station)
        (ensure-gethash (angle station asteroid)
                        angles
                        ())
        (push asteroid (gethash (angle station asteroid)
                                angles))))
    (let* ((angles-sorted (copy-hash-table angles
                                           :key (curry #'sort-distance
                                                       station)))
           (blacklist
             (loop :for angle :in (apply #'circular-list
                                         (sort (hash-table-keys angles)
                                               #'>))
                   :repeat (length asteroids)
                   :when (gethash angle angles-sorted)
                     :collect (pop (gethash angle angles-sorted)))))
      (values (nth 199 blacklist)
              (when debugp blacklist)
              angles-sorted))))

(defun angle (from to)
  ;; mirror to get π facing top
  (let+ ((#(dy dx) (map 'vector #'- to from)))
    (phase (complex dx dy))))

(defun sort-distance (from things)
  (sort things #'< :key (curry #'distance from)))

(defun distance (a b)
  (sqrt (loop :for ai :across a
              :for bi :across b
              :sum (expt (- bi ai)
                         2))))
