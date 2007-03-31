#|
Copyright 2006,2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

#+mp (progn

(defstruct condition-var
  "CMUCL doesn't have conditions, so we need to create our own type."
  lock
  active)

;;; Thread Creation

(defmethod make-thread (function &key name)
  (mp:make-process function :name name))

(defmethod current-thread ()
  mp:*current-process*)

(defmethod threadp (object)
  (mp:processp object))

(defmethod thread-name ((thread mp::process))
  (mp:process-name thread))

;;; Resource contention: locks and recursive locks

(defmethod make-lock (&optional name)
  (mp:make-lock name))

(defmethod acquire-lock ((lock mp:lock) &optional (wait-p t))
  (if wait-p
      (mp::lock-wait lock "Lock")
      (mp::lock-wait-with-timeout lock "Lock" 0)))

(defmethod release-lock ((lock mp:lock))
  (setf (mp::lock-process lock) nil))

(defmacro with-lock-held ((place) &body body)
  `(mp:with-lock-held (,place) ,@body))

(defmacro with-recursive-lock-held ((place &key timeout) &body body)
  `(mp:with-lock-held (,place "Lock Wait" :timeout ,timeout) ,@body))

;;; Note that the locks _are_ recursive, but not "balanced", and only
;;; checked if they are being held by the same process by with-lock-held.
;;; The default with-lock-held in bordeaux-mp.lisp sort of works, in that
;;; it will wait for recursive locks by the same process as well.

;;; Resource contention: condition variables

;;; There's some stuff in x86-vm.lisp that might be worth investigating
;;; whether to build on. There's also process-wait and friends.

(defmethod make-condition-variable ()
  (make-condition-var :lock (make-lock)))

(defmethod condition-wait ((condition-variable condition-var)
			   (lock mp:lock))
  (progn (setf (condition-var-active condition-variable) nil)
	 (release-lock lock)
	 (do ()
	     ((when (condition-var-active condition-variable)
		(acquire-lock lock)
		t))
	   (process-yield))))

(defmethod condition-notify ((condition-variable condition-var))
  (with-lock-held ((condition-var-lock condition-variable))
    (setf (condition-var-active condition-variable) t)))

(defmethod process-yield ()
  (mp:process-yield))

;;; Introspection/debugging

(defmethod all-threads ()
  (mp:all-processes))

(defmethod interrupt-thread ((thread mp:process) function)
  (mp:process-interrupt thread function))

(defmethod destroy-thread ((thread mp:process))
  (mp:destroy-process thread))

(mark-supported)
) ; end PROGN
