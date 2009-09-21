;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

(defvar *thread-join-mutex* nil)

;;; initialize *thread-join-mutex* for loading thread
;;; NB: all existing threads at time of loading(even those not created by B-T)
;;;     will become "joinable".
(eval-when (:load-toplevel)
  (mapcar
   (lambda (thr)
     (unless (mt:symbol-value-thread '*thread-join-mutex* thr)
       (mt:thread-interrupt
        thr
        :function #'mt:mutex-lock
        :arguments (list (setf (mt:symbol-value-thread '*thread-join-mutex* T)
                               (mt:make-mutex))))))
   (mt:list-threads)))

;;; Thread Creation
(defun %make-thread (function name)
  (mt:make-thread
   (lambda ()
     (let ((*thread-join-mutex* (mt:make-mutex)))
       (mt:with-mutex-lock (*thread-join-mutex*)
         (funcall function))))
   :name name
   :initial-bindings mt:*default-special-bindings*))

(defun current-thread ()
  (mt:current-thread))

(defun threadp (object)
  (mt:threadp object))

(defun thread-name (thread)
  (mt:thread-name thread))

;;; Resource contention: locks and recursive locks

(defun make-lock (&optional name)
  (mt:make-mutex :name (or name "Anonymous lock")))

(defun acquire-lock (lock &optional (wait-p t))
  (mt:mutex-lock lock :timeout (if wait-p nil 0)))

(defun release-lock (lock)
  (mt:mutex-unlock lock))

(defmacro with-lock-held ((place) &body body)
  `(mt:with-mutex-lock (,place) ,@body))

(defun make-recursive-lock (&optional name)
  (mt:make-mutex :name (or name "Anonymous recursive lock")
                 :recursive-p t))

(defmacro with-recursive-lock-held ((place) &body body)
  `(mt:with-mutex-lock (,place) ,@body))

;;; Resource contention: condition variables

(defun make-condition-variable (&key name)
  (mt:make-exemption :name (or name "Anonymous condition variable")))

(defun condition-wait (condition-variable lock)
  (mt:exemption-wait condition-variable lock))

(defun condition-notify (condition-variable)
  (mt:exemption-signal condition-variable))

(defun thread-yield ()
  (mt:thread-yield))

;;; Timeouts

;; VTZ: is there timeout-function (executed on timeout)?
;; How to distinguish between NIL returned from body and timeout ?
(defmacro with-timeout ((timeout) &body body)
  `(mt:with-timeout (,timeout nil)
     ,@body))

;;; Introspection/debugging

;;; VTZ: mt:list-threads returns all threads that are not garbage collected.
(defun all-threads ()
  (delete-if-not #'mt:thread-active-p (mt:list-threads)))

(defun interrupt-thread (thread function)
  (mt:thread-interrupt thread :function function))

(defun destroy-thread (thread)
  ;;; VTZ: actually we can kill ourselelf.
  ;;; suicide is part of our contemporary life :)
  (signal-error-if-current-thread thread)
  (mt:thread-interrupt thread :function t))

(defun thread-alive-p (thread)
  (mt:thread-active-p thread))

;;; VTZ: the current implementation is trivial and may cause contention
;;; if the thread is tried to be joined immediately after its creation
;;; or if :initial-bindings argument of make-thread cause entering the debugger
(defun thread-join (thread)
  (loop while (mt:thread-active-p thread) do
       (let ((jmx (mt:symbol-value-thread '*thread-join-mutex* thread)))
         (when jmx ; mutex may have not been created
           (mt:mutex-lock jmx) ; wait
           ; give chance other threads to wait/join as well
           (mt:mutex-unlock jmx)))))

(mark-supported)
