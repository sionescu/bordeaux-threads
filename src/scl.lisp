;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

#|
Copyright 2008 Scieneer Pty Ltd

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

(defun %make-thread (function name)
  (thread:thread-create function :name name))

(defun current-thread ()
  thread:*thread*)

(defun threadp (object)
  (typep object 'thread:thread))

(defun thread-name (thread)
  (thread:thread-name thread))

;;; Resource contention: locks and recursive locks

(defun make-lock (&optional name)
  (thread:make-lock (or name "Anonymous lock")))

(defun acquire-lock (lock &optional (wait-p t))
  (thread::acquire-lock lock nil wait-p))

(defun release-lock (lock)
  (thread::release-lock lock))

(defmacro with-lock-held ((place) &body body)
  `(thread:with-lock-held (,place) ,@body))

(defun make-recursive-lock (&optional name)
  (thread:make-lock (or name "Anonymous recursive lock")
                    :type :recursive))

;;; XXX acquire-recursive-lock and release-recursive-lock are actually
;;; complicated because we can't use control stack tricks.  We need to
;;; actually count something to check that the acquire/releases are
;;; balanced

(defmacro with-recursive-lock-held ((place) &body body)
  `(thread:with-lock-held (,place)
     ,@body))

;;; Resource contention: condition variables

(defun make-condition-variable (&key name)
  (thread:make-cond-var (or name "Anonymous condition variable")))

(defun condition-wait (condition-variable lock)
  (thread:cond-var-wait condition-variable lock))

(defun condition-notify (condition-variable)
  (thread:cond-var-broadcast condition-variable))

(defun thread-yield ()
  (mp:process-yield))

;;; Timeouts

(defmacro with-timeout ((timeout) &body body)
  `(error "with-timeout is not reliable and should not be used."))

;;; Introspection/debugging

(defun all-threads ()
  (mp:all-processes))

(defun interrupt-thread (thread function)
  (thread:thread-interrupt thread function))

(defun destroy-thread (thread)
  (thread:destroy-thread thread))

(defun thread-alive-p (thread)
  (mp:process-alive-p thread))

(defun join-thread (thread)
  (mp:process-wait (format nil "Waiting for thread ~A to complete" thread)
                   (lambda () (not (mp:process-alive-p thread)))))

(mark-supported)
