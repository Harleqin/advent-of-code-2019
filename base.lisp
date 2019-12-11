(in-package #:cl-user)

(defpackage #:aoc-2019
  (:use #:cl
        #:alexandria
        #:arrows
        #:cl-ppcre
        #:for
        #:let-plus
        #:split-sequence)
  (:export #:array-flat-view
           #:defenum
           #:doto
           #:dovector
           #:factorize
           #:frequencies
           #:print-matrix
           #:read-integers
           #:read-matrix
           #:sort-by
           #:strcat))

(in-package #:aoc-2019)

(defun read-integers (filename)
  (with-open-file (in filename)
    (loop :for line := (read-line in nil)
          :while line
          :append (loop :for s := (substitute #\space #\, line)
                        :for (i pos) := (multiple-value-list
                                         (parse-integer s
                                                        :start (or pos 0)
                                                        :junk-allowed t))
                        :while i
                        :collect i))))

(defun read-matrix (filename)
  (let* ((lines (with-open-file (in filename)
                  (loop :for line := (read-line in nil)
                        :while line
                        :collect line)))
         (width (length (first lines)))
         (height (length lines))
         (array (make-array (list height width))))
    (loop :for y :below height
          :for line :in lines
          :do (loop :for x :below width
                    :for char :across line
                    :do (setf (aref array y x) char)))
    array))

(defun print-matrix (matrix lookup-alist)
  (dotimes (y (array-dimension matrix 0))
    (dotimes (x (array-dimension matrix 1))
      (princ (cdr (assoc (aref matrix y x) lookup-alist))))
    (terpri)))

(defmacro dovector ((var vector &optional return) &body body)
  `(loop :for ,var :across ,vector
         :do (tagbody
                ,@body)
         :finally (return ,return)))

(defun array-flat-view (array)
  (make-array (array-total-size array)
              :element-type (array-element-type array)
              :displaced-to array))

(defun frequencies (sequence &key (test #'eql))
  (let ((fs (make-hash-table :test test)))
    (for ((x over sequence))
      (incf (gethash x fs 0)))
    fs))

(defgeneric partition-by (f sequence &key test)
  (:documentation "Applies F to each item in SEQUENCE, splitting it each time F
  returns a new value.  Returns a list of sequences of the same type as
  SEQUENCES."))

(defmethod partition-by (f (sequence cons) &key (test #'eql))
  (loop :with bag := (list (first sequence))
        :with result := ()
        :for x :in (rest sequence)
        :for previous := (funcall f (first sequence)) :then current
        :for current := (funcall f x)
        :unless (funcall test previous current)
          :do (push (reverse bag) result)
              (setf bag ())
        :do (push x bag)
        :finally (push (reverse bag) result)
                 (return (reverse result))))

(defmethod partition-by (f (sequence vector) &key (test #'eql))
  (loop :with start := 0
        :with result := ()
        :for i :from 1 :below (length sequence)
        :for x := (aref sequence i)
        :for previous := (funcall f (aref sequence 0)) :then current
        :for current := (funcall f x)
        :unless (funcall test previous current)
          :do (push (subseq sequence start i) result)
              (setf start i)
        :finally (push (subseq sequence start) result)
                 (return (reverse result))))

(defun factorize (n)
  (let ((factors ()))
    (loop :while (evenp n)
          :do (push 2 factors)
              (setf n (/ n 2)))
    (loop :with f := 3
          :while (and (> n 1)
                      (<= f n))
          :do (if (zerop (mod n f))
                  (progn (push f factors)
                         (setf n (/ n f)))
                  (incf f 2)))
    factors))

(defmacro defenum (&body elements)
  `(progn
     ,@(mapcar (lambda (e i)
                 `(defconstant ,e ,i))
               elements
               (iota (length elements)))))

(defmacro sort-by (sequence arglist &rest more-arglists)
  "Sorts a sequence by several attributes, given in ARGLISTS.  ARGLISTS are
triplets of comparator, key function, and equality test.  For each comparison of
the sort, each triplet is tried in sequence, until the key function of one gives
results not equal under the equality test; then, the result of the comparison is
the result of the comparator to the results of that key function.  The equality
test of the last arglist is ignored."
  `(sort-by% ,sequence
             ,@(mapcar (lambda (triplet)
                         (cons 'list triplet))
                       (cons arglist more-arglists))))

(defun sort-by% (sequence &rest arglists)
  (setf (cddar (last arglists))
        (list (constantly nil)))
  (sort sequence
        (lambda (a b)
          (loop :for (comp key eqtest) :in arglists
                :for a-val := (funcall key a)
                :for b-val := (funcall key b)
                :unless (funcall eqtest a-val b-val)
                  :return (funcall comp a-val b-val)))))

(defun strcat (&rest things)
  (with-output-to-string (out)
    (dolist (thing things)
      (princ thing out))))

(defmacro doto (var form &body body)
  "Binds VAR to FORM in BODY, returns value of VAR."
  `(let ((,var ,form))
     ,@body
     ,var))
