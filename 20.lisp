(in-package #:aoc-2019)

(defpackage #:aoc-2019/20
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:cl-ppcre
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/20)

(defun aoc20a (&optional (map (read-map (read-file-into-string "20"))))
  (let ((marks (make-hash-table :test #'equal))
        (neighbours (make-hash-table :test #'equalp)))
    (loop :for y :below (array-dimension map 0)
          :do (loop :for x :below (array-dimension map 1)
                    :for c := (aref map y x)
                    :when (char= c #\.)
                      :do (let+ ((pos (vector x y))
                                 ((&values nicks mark) (neighbours map pos)))
                            (setf (gethash pos neighbours)
                                  nicks)
                            (when mark
                              (push pos (gethash mark marks))))))
    ;; connect portals
    (maphash (lambda (pos nicks)
               (loop :for i :upfrom 0
                     :for n :in nicks
                     :when (stringp n)
                       :do (setf (nth i nicks)
                                 (or (find pos (gethash n marks)
                                           :test-not #'equalp)
                                     (error "mark not found: ~s in ~s"
                                            n marks)))))
             neighbours)
    (values (loop :with done := (make-hash-table :test #'equalp)
                  :with zz := (first (gethash "ZZ" marks))
                  :for dist :upfrom 0
                  :for open := (gethash "AA" marks)
                    :then (handler-case (mapcan (curry #'expand neighbours done zz)
                                                open)
                            (found-zz ()
                              (return dist)))
                  :while open)
            marks
            neighbours)))

(define-condition found-zz () ())

(defun expand (neighbours done zz pos)
  (setf (gethash pos done) t)
  (loop :for nick :in (gethash pos neighbours)
        :when (equalp zz nick)
          :do (signal 'found-zz)
        :unless (gethash nick done)
          :collect nick))

(defmacro all-dirs ()
  `(list #(1 0) #(0 1) #(-1 0) #(0 -1)))

(defun neighbours (map pos)
  (loop :with mark := nil
        :for d :in (all-dirs)
        :for p := (map 'vector #'+ pos d)
        :for c1 := (map-ref map p)
        :when (char= c1 #\.)
          :collect p :into nicks
        :when (upper-case-p c1)
          :collect (let* ((c2 (map-ref map (map 'vector #'+ p d)))
                          (marc (coerce (vector c1 c2) 'string)))
                     (setf mark (if (find -1 d)
                                    (reverse marc)
                                    marc)))
            :into nicks
        :finally (return (values (set-difference nicks
                                                 '("AA" "ZZ")
                                                 :test #'equal)
                                 mark))))
