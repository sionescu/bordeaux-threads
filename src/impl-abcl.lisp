;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

;;; the implementation of the Armed Bear thread interface can be found in
;;; src/org/armedbear/lisp/LispThread.java

(deftype thread ()
  'ext:thread)

;;; Thread Creation

(defun %make-thread (function name)
  (threads:make-thread function :name name))

(defun current-thread ()
  (threads:current-thread))

(defun thread-name (thread)
  (threads:thread-name thread))

(defun threadp (object)
  (typep object 'thread))

;;; Resource contention: locks and recursive locks

(defun make-lock (&optional name)
  (declare (ignore name))
  (threads:make-thread-lock))

(defun acquire-lock (lock &optional (wait-p t))
  (declare (ignore wait-p))
  (threads:thread-lock lock))

(defun release-lock (lock)
  (threads:thread-unlock lock))

(defmacro with-lock-held ((place) &body body)
  `(threads:with-thread-lock (,place) ,@body))

;;; Resource contention: condition variables

(defun thread-yield ()
  (sleep 0))

;;; Introspection/debugging

(defun all-threads ()
  (let ((threads ()))
    (threads:mapcar-threads (lambda (th)
			      (push th threads)))
    (reverse threads)))

(defun interrupt-thread (thread function &rest args)
  (apply #'threads:interrupt-thread thread function args))

(defun destroy-thread (thread)
  (signal-error-if-current-thread thread)
  (threads:destroy-thread thread))

(defun thread-alive-p (thread)
  (threads:thread-alive-p thread))

(defun join-thread (thread)
  (threads:thread-join thread))

(mark-supported)
