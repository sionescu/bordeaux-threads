#|
Copyright 2006,2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(defpackage bordeaux-threads-test
  (:use #:cl #:bordeaux-threads #:lift)
  (:shadow #:with-timeout))

(in-package #:bordeaux-threads-test)

(deftestsuite test-bordeaux-threads ()
  ((lock :initform (make-lock))))

(addtest should-have-current-thread
  (ensure (current-thread)))

(addtest should-identify-threads-correctly
  (ensure (threadp (current-thread)))
  (ensure (threadp (make-thread (lambda () t) :name "foo")))
  (ensure (not (threadp (make-lock)))))

(addtest should-retrieve-thread-name
  (ensure-same (thread-name (make-thread (lambda () t) :name "foo")) "foo"))

(addtest should-lock-without-contention
  (ensure (acquire-lock lock t))
  (release-lock lock)
  (ensure (acquire-lock lock nil))
  (release-lock lock))

(defparameter *shared* 0)
(defparameter *lock* (make-lock))

(addtest should-have-thread-interaction
  ;; this simple test generates N process. Each process grabs and
  ;; releases the lock until SHARED has some value, it then
  ;; increments SHARED. the outer code first sets shared 1 which
  ;; gets the thing running and then waits for SHARED to reach some
  ;; value. this should, i think, stress test locks.
  (setf *shared* 0)
  (dotimes (i 1)
    (let* ((procs (loop
                     for i from 1 upto 2
                     collect (make-thread
                              (compile nil
                                       `(lambda ()
                                          (loop
                                             named wait
                                             do (with-lock-held (*lock*)
                                                  (when (= ,i *shared*)
                                                    (incf *shared*)
                                                    (return-from wait))))))
                              :name (format nil "Proc #~D" i)))))
      (with-lock-held (*lock*)
        (incf *shared*))
      (block test
        (loop
           until (with-lock-held (*lock*)
                   (= (1+ (length procs)) *shared*))
           do (with-lock-held (*lock*)
                (ensure (>= (1+ (length procs)) *shared*))))))))

(defparameter *condition-variable* (make-condition-variable))

(addtest condition-variable
  (setf *shared* 0)
  (let ((num-procs 100))
    (dotimes (i num-procs)
      (make-thread
       (compile nil
                `(lambda ()
                   (with-lock-held (*lock*)
                     (loop
                        until (= ,i *shared*)
                        do (condition-wait *condition-variable* *lock*))
                     (incf *shared*))
                   (condition-notify *condition-variable*)))))
    (with-lock-held (*lock*)
      (loop
         until (= num-procs *shared*)
         do (condition-wait *condition-variable* *lock*)))
    (ensure-same num-procs *shared*)))