(in-package #:cl-user)

(defpackage #:aoc-2019
  (:use #:cl
        #:alexandria
        #:arrows
        #:cl-ppcre
        #:for
        #:let-plus
        #:split-sequence)
  (:export #:2d-pos
           #:2d-pos-if
           #:array-flat-view
           #:defenum
           #:doto
           #:dovector
           #:extrapolate
           #:factorize
           #:frequencies
           #:map-ref
           #:print-matrix
           #:read-integers
           #:read-map
           #:read-matrix
           #:repeat
           #:sort-by
           #:string-lines
           #:strcat)
  (:export #:unique-heap
           #:make-unique-heap
           #:heap-upsert
           #:heap-pop))

(in-package #:aoc-2019)

(defun read-integers (filename &optional (type 'list))
  (coerce (with-open-file (in filename)
            (loop :for line := (read-line in nil)
                  :while line
                  :append (loop :for s := (substitute #\space #\, line)
                                :for (i pos)
                                  := (multiple-value-list
                                      (parse-integer s
                                                     :start (or pos 0)
                                                     :junk-allowed t))
                                :while i
                                :collect i)))
          type))

(defun read-matrix (lines)
  (let* ((width (length (first lines)))
         (height (length lines))
         (array (make-array (list height width))))
    (loop :for y :below height
          :for line :in lines
          :do (loop :for x :below width
                    :for char :across line
                    :do (setf (aref array y x) char)))
    array))

(defun read-map (view)
  (read-matrix (string-lines view)))

(defun string-lines (string)
  (with-input-from-string (v string)
    (loop :for line := (read-line v nil)
          :while (some-> line length plusp)
          :collect line)))

(defun print-matrix (matrix &optional lookup-alist)
  (flet ((lookup (e)
           (if lookup-alist
               (cdr (assoc e lookup-alist))
               e)))
    (dotimes (y (array-dimension matrix 0))
      (dotimes (x (array-dimension matrix 1))
        (princ (lookup (aref matrix y x))))
      (terpri))))

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

;; A wrapper for a heap that keeps track of already present values.

(defstruct unique-heap
  (heap (make-instance 'pairing-heap:pairing-heap))
  (nodes (make-hash-table :test #'equalp)))

(defun heap-upsert (uheap key value)
  "Inserts VALUE with priority given by KEY into the UHEAP.  If VALUE is already
in UHEAP, its existing key is instead decreased to KEY if that is lower.
Returns two values: whether an update took place (insert or key-decrease), and
the effective key afterwards (the inserted, decreased or existing key)."
  (let+ (((&structure unique-heap- heap nodes) uheap))
    (if-let ((node (gethash value nodes)))
      (if (< key (pairing-heap::node-key node))
          (progn (pairing-heap:decrease-key heap node key)
                 (values t key))
          (values nil (pairing-heap::node-key node)))
      (progn (setf (gethash value nodes)
                   (pairing-heap:insert heap key value))
             (values t key)))))

(defun heap-pop (uheap)
  (let+ (((&structure unique-heap- heap nodes) uheap))
    (unless (pairing-heap:empty-p heap)
      (let ((value (pairing-heap:extract-min heap)))
        (remhash value nodes)
        value))))

;;

(defgeneric repeat (n sequence))

(defmethod repeat (n (sequence vector))
  (let ((out (make-array (* n (length sequence)))))
    (loop :for start :below (length out) :by (length sequence)
          :do (loop :for i :below (length sequence)
                    :do (setf (aref out (+ start i))
                              (aref sequence i))))
    out))

(defun 2d-pos (item map &key (test #'eql))
  (loop :for y :below (array-dimension map 0)
        :do (loop :for x :below (array-dimension map 1)
                  :for e := (aref map y x)
                  :when (funcall test item e)
                    :do (return-from 2d-pos
                          (values (vector x y) e)))))

(defun 2d-pos-if (f map)
  (loop :for y :below (array-dimension map 0)
        :do (loop :for x :below (array-dimension map 1)
                  :for e := (aref map y x)
                  :when (funcall f e)
                    :do (return-from 2d-pos-if
                          (values (vector x y) e)))))

(defmacro map-ref (map pos)
  `(aref ,map (aref ,pos 1) (aref ,pos 0)))

(defun extrapolate (v a b)
  (+ (* (- 1 v) a) (* v b)))
