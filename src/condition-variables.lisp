#|
Copyright 2006,2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

;;; This file provides a portable implementation of condition
;;; variables (given a working WITH-LOCK-HELD and THREAD-YIELD), and
;;; should be used if there is no condition variable implementation in
;;; the host Lisp.

(defclass condition-var ()
  ((lock :initarg :lock :reader condition-var-lock)
   (active :accessor condition-var-active)))

(defmethod make-condition-variable ()
  (make-instance 'condition-var :lock (make-lock)))

(defmethod condition-wait ((condition-variable condition-var) lock)
  (progn (setf (condition-var-active condition-variable) nil)
	 (release-lock lock)
	 (do ()
	     ((when (condition-var-active condition-variable)
		(acquire-lock lock)
		t))
	   (thread-yield))))

(defmethod condition-notify ((condition-variable condition-var))
  (with-lock-held ((condition-var-lock condition-variable))
    (setf (condition-var-active condition-variable) t)))
