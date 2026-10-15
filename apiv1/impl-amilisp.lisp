;;;; -*- indent-tabs-mode: nil -*-

;;;; AmiLisp backend for bordeaux-threads API v1.
;;;; Uses the MP package provided by AmiLisp's threading primitives.

(in-package #:bordeaux-threads)

;;; Thread Creation

(deftype thread ()
  t)

(defun %make-thread (function name)
  (mp:make-thread function :name name))

(defun current-thread ()
  (mp:current-thread))

(defun threadp (object)
  (mp:threadp object))

(defun thread-name (thread)
  (mp:thread-name thread))

;;; Resource contention: locks and recursive locks

(deftype lock () t)

(deftype recursive-lock () t)

(defun lock-p (object)
  (mp:lockp object))

(defun recursive-lock-p (object)
  (mp:lockp object))

(defun make-lock (&optional name)
  (mp:make-lock (or name "Anonymous lock")))

(defun acquire-lock (lock &optional (wait-p t))
  (mp:acquire-lock lock wait-p))

(defun release-lock (lock)
  (mp:release-lock lock))

(defmacro with-lock-held ((place) &body body)
  `(mp:with-lock-held (,place) ,@body))

(defun make-recursive-lock (&optional name)
  (mp:make-recursive-lock (or name "Anonymous recursive lock")))

(defun acquire-recursive-lock (lock &optional (wait-p t))
  (mp:acquire-lock lock wait-p))

(defun release-recursive-lock (lock)
  (mp:release-lock lock))

(defmacro with-recursive-lock-held ((place) &body body)
  `(mp:with-lock-held (,place) ,@body))

;;; Resource contention: condition variables

(defun make-condition-variable (&key name)
  (mp:make-condition-variable name))

(defun condition-wait (condition-variable lock &key timeout)
  (if timeout
      (mp:condition-wait condition-variable lock timeout)
      (mp:condition-wait condition-variable lock)))

(defun condition-notify (condition-variable)
  (mp:condition-notify condition-variable))

(defun thread-yield ()
  (mp:thread-yield))

;;; Introspection/debugging

(defun all-threads ()
  (mp:all-threads))

(defun interrupt-thread (thread function &rest args)
  (declare (ignore args))
  (mp:interrupt-thread thread function))

(defun destroy-thread (thread)
  (mp:destroy-thread thread))

(defun thread-alive-p (thread)
  (mp:thread-alive-p thread))

(defun join-thread (thread)
  (mp:join-thread thread))

(mark-supported)
