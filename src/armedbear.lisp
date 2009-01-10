;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

;;; the implementation of the Armed Bear thread interface can be found in
;;; src/org/armedbearlisp/LispThread.java

;;; Thread Creation

(defun %make-thread (function name)
  (ext:make-thread function :name name))

(defun current-thread ()
  (ext:current-thread))

(defun thread-name (thread)
  (ext:thread-name thread))

;;; Yes, this is nasty
(defun threadp (object)
  (handler-case (progn (thread-name object) t)
    (type-error () nil)))

;;; Resource contention: locks and recursive locks

;;; Don't know what the arguments to MAKE-THREAD-LOCK are, but it
;;; doesn't mind being a thunk
(defun make-lock (&optional name)
  (declare (ignore name))
  (ext:make-thread-lock))

(defun acquire-lock (lock &optional (wait-p t))
  (declare (ignore wait-p))
  (ext:thread-lock lock))

(defun release-lock (lock)
  (ext:thread-unlock lock))

(defmacro with-lock-held ((place) &body body)
  `(ext:with-thread-lock (,place) ,@body))

;;; Resource contention: condition variables

(defun thread-yield ()
  (sleep 0))

;;; Introspection/debugging

(defun interrupt-thread (thread function)
  (ext:interrupt-thread thread function))

(defun destroy-thread (thread)
  (signal-error-if-current-thread thread)
  (ext:destroy-thread thread))

(defun thread-alive-p (thread)
  (ext:thread-alive-p thread))

(mark-supported)
