;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

;;; Thread Creation

(defun %make-thread (function name)
  (mt:make-thread function :name name))

(defun current-thread ()
  (mt:current-thread))

(defun threadp (object)
  (mt:threadp object))

(defun thread-name (thread)
  (mt:thread-name thread))

;;; Resource contention: locks and recursive locks

(defun make-lock (&optional name)
  (mt:make-mutex :name name))

(defun acquire-lock (lock &optional (wait-p t))
  (declare (ignore wait-p))
  (mt:mutex-lock lock))

(defun release-lock (lock)
  (mt:mutex-unlock lock))

(defmacro with-lock-held ((place) &body body)
  `(mt:with-lock (,place) ,@body))

(defun make-recursive-lock (&optional name)
  (mt:make-mutex :name name :recursive-p t))

;;; XXX acquire-recursive-lock and release-recursive-lock are actually
;;; complicated because we can't use control stack tricks.  We need to
;;; actually count something to check that the acquire/releases are
;;; balanced

(defmacro with-recursive-lock-held ((place) &body body)
  `(mt:with-lock (,place) ,@body))

;;; Resource contention: condition variables

(defun make-condition-variable ()
  (mt::make-exemption))

(defun condition-wait (condition-variable lock)
  (mt:exemption-wait condition-variable lock))

(defun condition-notify (condition-variable)
  (mt:exemption-signal condition-variable))

(defun thread-yield ()
  (mt:thread-yield))

;;; Timeouts

(defmacro with-timeout ((timeout) &body body)
  `(mt:with-timeout (,timeout)
     ,@body))

;;; Introspection/debugging

(defun all-threads ()
  (mt:list-threads))

(defun interrupt-thread (thread function)
  (mt:thread-interrupt thread function))

(defun destroy-thread (thread)
  (signal-error-if-current-thread thread)
  (mt:thread-kill thread))

(defun thread-alive-p (thread)
  (mt:thread-active-p thread))

(mark-supported)
