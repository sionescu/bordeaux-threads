;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

;;; Thread Creation

(defun %make-thread (function name)
  (ccl:process-run-function name function))

(defun current-thread ()
  ccl:*current-thread*)

(defun threadp (object)
  (ccl:processp object))

(defun thread-name (thread)
  (ccl:process-name thread))

;;; Resource contention: locks and recursive locks

(defun make-lock (&optional name)
  (ccl:make-lock name))

(defmacro with-lock-held ((place) &body body)
  `(ccl:with-lock-grabbed (,place) ,@body))

(defun thread-yield ()
  (ccl:process-allow-schedule))

;;; Introspection/debugging

(defun all-threads ()
  ccl:*all-processes*)

(defun interrupt-thread (thread function)
  (ccl:process-interrupt thread function))

(defun destroy-thread (thread)
  (signal-error-if-current-thread thread)
  (ccl:process-kill thread))

(mark-supported)
