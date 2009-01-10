;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

;;; documentation on the Allegro Multiprocessing interface can be found at
;;; http://www.franz.com/support/documentation/8.1/doc/multiprocessing.htm

;;; Thread Creation

(defun %make-thread (function name)
  (mp:process-run-function name function))

(defun current-thread ()
  mp:*current-process*)

(defun threadp (object)
  (typep object 'mp:process))

(defun thread-name (thread)
  (mp:process-name thread))

;;; Resource contention: locks and recursive locks

(defun make-lock (&optional name)
  (mp:make-process-lock :name name))

(defun acquire-lock (lock &optional (wait-p t))
  (mp:process-lock lock mp:*current-process* "Lock" (if wait-p nil 0)))

(defun release-lock (lock)
  (mp:process-unlock lock))

(defmacro with-lock-held ((place) &body body)
  `(mp:with-process-lock (,place :norecursive t)
     ,@body))

(defmacro with-recursive-lock-held ((place &key timeout) &body body)
  `(mp:with-process-lock (,place :timeout ,timeout)
     ,@body))

;;; Resource contention: condition variables

(defun make-condition-variable ()
  (mp:make-gate nil))

(defun condition-wait (condition-variable lock)
  (release-lock lock)
  (mp:process-wait "wait for message" #'mp:gate-open-p condition-variable)
  (acquire-lock lock)
  (mp:close-gate condition-variable))

(defun condition-notify (condition-variable)
  (mp:open-gate condition-variable))

(defun thread-yield ()
  (mp:process-allow-schedule))

;;; Timeouts

(defmacro with-timeout ((timeout) &body body)
  `(mp:with-timeout (,timeout)
     ,@body))

;;; Introspection/debugging

(defun all-threads ()
  mp:*all-processes*)

(defun interrupt-thread (thread function)
  (mp:process-interrupt thread function))

(defun destroy-thread (thread)
  (signal-error-if-current-thread thread)
  (mp:process-kill thread))

(defun thread-alive-p (thread)
  (mp:process-alive-p thread))

(mark-supported)
