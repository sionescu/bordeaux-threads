;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

;;; documentation on the LispWorks Multiprocessing interface can be found at
;;; http://www.lispworks.com/documentation/lw445/LWUG/html/lwuser-156.htm

(mp:initialize-multiprocessing)

;;; Thread Creation

(defun %make-thread (function name)
  (mp:process-run-function name nil function))

(defun current-thread ()
  mp:*current-process*)

(defun threadp (object)
  (typep object 'mp:process))

(defun thread-name (thread)
  (mp:process-name thread))

;;; Resource contention: locks and recursive locks

(defun make-lock (&optional name)
  (mp:make-lock :name name))

(defun acquire-lock (lock &optional (wait-p t))
  (mp:process-lock lock nil (if wait-p
                                (if (typep wait-p 'number) wait-p nil)
                                0)))

(defun release-lock (lock)
  (mp:process-unlock lock))

;;; Apparently this EVAL-WHEN is needed so that the macro is available
;;; when compiling condition-variables.lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-lock-held ((place) &body body)
    `(mp:with-lock (,place) ,@body)))

;;; Resource contention: condition variables

(defun thread-yield ()
  (mp:process-allow-scheduling))

;;; Introspection/debugging

(defun all-threads ()
  (mp:list-all-processes))

(defun interrupt-thread (thread function)
  (mp:process-interrupt thread function))

(defun destroy-thread (thread)
  (signal-error-if-current-thread thread)
  (mp:process-kill thread))

(defun thread-alive-p (thread)
  (mp:process-alive-p thread))

(mark-supported)
