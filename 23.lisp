(in-package #:aoc-2019)

(defpackage #:aoc-2019/23
  (:use #:cl
        #:alexandria
        #:aoc-2019
        #:arrows
        #:cl-ppcre
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2019/23)

(defun aoc23a ()
  (let* ((nic (read-integers "23" 'vector))
         (routers (coerce (loop :for address :from 0
                                :repeat 50
                                :collect (create-router nic address))
                          'vector)))
    (loop :for addr := 0 :then (mod (1+ addr) 50)
          :for (to x y) := (recv (aref routers addr))
          :when to
            :do (format t "~a -> ~a: ~a, ~a~%"
                        addr to x y)
                (if (= 255 to)
                    (return y)
                    (send (aref routers to) x y)))))

(defstruct (router
            (:constructor make-router
                (addr
                 &aux
                   (packet-buffer addr)
                   (in-ch (make-instance 'chanl:unbounded-channel))
                   (out-ch (make-instance 'chanl:unbounded-channel)))))
  packet-buffer
  in-ch
  out-ch
  idle-since)

(defun create-router (nic addr)
  (let ((router (make-router addr)))
    (flet ((router-in ()
             (let ((input (cond ((router-packet-buffer router)
                                 (shiftf (router-packet-buffer router) nil))
                                ((chanl:recv (router-in-ch router)
                                             :blockp nil))
                                (t -1))))
               (unless (eql input -1)
                 (setf (router-idle-since router) (local-time:now)))
               (ctypecase input
                 (integer input)
                 (cons
                  (setf (router-packet-buffer router) (second input))
                  (first input)))))
           (router-out (n)
             (setf (router-idle-since router) (local-time:now))
             (chanl:send (router-out-ch router) n)))
      (chanl:pexec ()
        (aoc-2019/intcode:intcode-core nic
                                       #'router-in
                                       #'router-out))
      router)))

(defun send (router x y)
  (chanl:send (router-in-ch router)
              (list x y)))

(defun recv (router)
  (let ((addr (chanl:recv (router-out-ch router)
                          :blockp nil)))
    (when addr
      (list addr
            (chanl:recv (router-out-ch router))
            (chanl:recv (router-out-ch router))))))

(defun idlep (router)
  (some-> (router-idle-since router)
          (local-time:timestamp-difference (local-time:now))
          -
          (> 1)))

(defun aoc23b ()
  (let* ((nic (read-integers "23" 'vector))
         (routers (coerce (loop :for address :from 0
                                :repeat 50
                                :collect (create-router nic address))
                          'vector))
         (nat nil)
         (released nil))
    (loop :for addr := 0 :then (mod (1+ addr) 50)
          :for (to x y) := (recv (aref routers addr))
          :when to
            :do (format t "~a -> ~a: ~a, ~a~%"
                        addr to x y)
                (if (= 255 to)
                    (setf nat (list x y))
                    (send (aref routers to) x y))
          :when (and nat (every #'idlep routers))
            :do (if (eql (second nat) (second released))
                    (return (second nat))
                    (progn
                      (format t "IDLE, reset with ~a~%" nat)
                      (setf released nat)
                      (dovector (router routers)
                        (setf (router-idle-since router) (local-time:now)))
                      (apply #'send (aref routers 0) nat))))))
