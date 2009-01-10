;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

;;; documentation on the ECL Multiprocessing interface can be found at
;;; http://ecls.sourceforge.net/cgi-bin/view/Main/MultiProcessing

;;; Thread Creation

(defun %make-thread (function name)
  (mp:process-run-function name function))

(defun current-thread ()
  mp::*current-process*)

(defun threadp (object)
  (typep object 'mp:process))

(defun thread-name (thread)
  (mp:process-name thread))

;;; Resource contention: locks and recursive locks

(defun make-lock (&optional name)
  (mp:make-lock :name name))

(defun acquire-lock (lock &optional (wait-p t))
  (mp:get-lock lock wait-p))

(defun release-lock (lock)
  (mp:giveup-lock lock))

(defmacro with-lock-held ((place) &body body)
  `(mp:with-lock (,place) ,@body))

;; FIXME: Missing:
;;        * make-recursive-lock
;;        * acquire-recursive-lock
;;        * release-recursive-lock

;;; Resource contention: condition variables

(defun thread-yield ()
  ;; (mp:yield)
  (sleep 0))

;;; Introspection/debugging

(defun all-threads ()
  (mp:all-processes))

(defun interrupt-thread (thread function)
  (mp:interrupt-process thread function))

(defun destroy-thread (thread)
  (signal-error-if-current-thread thread)
  (mp:process-kill thread))

(defun thread-alive-p (thread)
  (mp:process-active-p thread))

(mark-supported)
