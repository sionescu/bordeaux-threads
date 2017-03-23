;;;; -*- indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

;;; documentation on the OpenMCL Threads interface can be found at
;;; http://openmcl.clozure.com/Doc/Programming-with-Threads.html

(deftype thread ()
  'ccl:process)

;;; Thread Creation

(defun %make-thread (function name)
  (ccl:process-run-function name function))

(defun current-thread ()
  ccl:*current-process*)

(defun threadp (object)
  (typep object 'ccl:process))

(defun thread-name (thread)
  (ccl:process-name thread))

;;; Resource contention: locks and recursive locks

(deftype lock () 'ccl:lock)

(deftype recursive-lock () 'ccl:lock)

(defun lock-p (object)
  (typep object 'ccl:lock))

(defun recursive-lock-p (object)
  (typep object 'ccl:lock))

(defun make-lock (&optional name)
  (ccl:make-lock (or name "Anonymous lock")))

(defun acquire-lock (lock &optional (wait-p t))
  (if wait-p
      (ccl:grab-lock lock)
      (ccl:try-lock lock)))

(defun release-lock (lock)
  (ccl:release-lock lock))

(defmacro with-lock-held ((place) &body body)
  `(ccl:with-lock-grabbed (,place)
     ,@body))

(defun make-recursive-lock (&optional name)
  (ccl:make-lock (or name "Anonymous recursive lock")))

(defun acquire-recursive-lock (lock)
  (ccl:grab-lock lock))

(defun release-recursive-lock (lock)
  (ccl:release-lock lock))

(defmacro with-recursive-lock-held ((place) &body body)
  `(ccl:with-lock-grabbed (,place)
     ,@body))

;;; Resource contention: condition variables
;;
;; Implementation of the condition variables with semaphores.
;;
;; The condition variables on top of semaphores implementation
;; strategies (both correct and incorrect ones) are described
;; in great detail in the following paper:
;;
;; Implementing Condition Variables with Semaphores, Andrew Birrell (Microsoft Research)
;; (https://www.microsoft.com/en-us/research/publication/implementing-condition-variables-with-semaphores/)
;; 

(defclass condition-variable ()
  ((waiters :type 'fixnum
            :initform 0)
   (waiters-lock :type 'lock
                 :initform (make-lock))
   (handshake-semaphore :type 'ccl:semaphore
                        :initform (ccl:make-semaphore))
   (signal-semaphore :type 'ccl:semaphore
                     :initform (ccl:make-semaphore))))

(defun make-condition-variable (&key name)
  (declare (ignore name))
  (make-instance 'condition-variable))

(defun condition-wait (condition-variable lock &key timeout)
  (with-slots (waiters
               waiters-lock
               handshake-semaphore
               signal-semaphore) condition-variable
    (with-lock-held (waiters-lock)
      (incf waiters))
    (release-lock lock)
    (unwind-protect
         (cond
           (timeout (if (ccl:timed-wait-on-semaphore signal-semaphore timeout)
                        ;; successful - wake handshake semaphore
                        (ccl:signal-semaphore handshake-semaphore)
                        ;; unsuccessful wait - decrement counter
                        (with-lock-held (waiters-lock)
                          (decf waiters))))
           (t (ccl:wait-on-semaphore signal-semaphore)
              (ccl:signal-semaphore handshake-semaphore)))
      (acquire-lock lock t))))

(defun condition-notify (condition-variable)
  (with-slots (waiters
               waiters-lock
               handshake-semaphore
               signal-semaphore) condition-variable
    (with-lock-held (waiters-lock)
      (when (plusp waiters)
        (decf waiters)
        (ccl:signal-semaphore signal-semaphore)
        (ccl:wait-on-semaphore handshake-semaphore)))))

(defun thread-yield ()
  (ccl:process-allow-schedule))

;;; Introspection/debugging

(defun all-threads ()
  (ccl:all-processes))

(defun interrupt-thread (thread function &rest args)
  (declare (dynamic-extent args))
  (apply #'ccl:process-interrupt thread function args))

(defun destroy-thread (thread)
  (signal-error-if-current-thread thread)
  (ccl:process-kill thread))

(defun thread-alive-p (thread)
  (not (ccl:process-exhausted-p thread)))

(defun join-thread (thread)
  (ccl:join-process thread))

(mark-supported)
