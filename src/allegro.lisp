#|
Copyright 2006,2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

;;; documentation on the Allegro Multiprocessing interface can be found at
;;; http://www.franz.com/support/documentation/6.2/doc/multiprocessing.htm

(eval-when (:compile-top-level :load-top-level :execute)
  (require :process))

#+multiprocessing (progn

;;; Thread Creation

(defmethod make-thread (function &key name)
  (mp:process-run-function name function))

(defmethod current-thread ()
  mp:*current-process*)

(defmethod threadp ((object mp:process))
  t)

(defmethod thread-name ((thread mp:process))
  (mp:process-name thread))

;;; Resource contention: locks and recursive locks

(defmethod make-lock (&optional name)
  (mp:make-process-lock :name name))

(defmethod acquire-lock ((lock mp:process-lock) &optional (wait-p t))
  (mp:process-lock lock mp:*current-process* "Lock" (if wait-p nil 0)))

(defmethod release-lock ((lock mp:process-lock))
  (mp:process-unlock lock))

(defmacro with-lock-held ((place) &body body)
  `(mp:with-process-lock (,place :norecursive t)
     ,@body))

(defmacro with-recursive-lock-held ((place &key timeout) &body body)
  `(mp:with-process-lock (,place :timeout ,timeout)
     ,@body))

;;; Resource contention: condition variables

(defmethod make-condition-variable ()
  (mp:make-gate nil))

(defmethod condition-wait ((condition-variable vector)
                           (lock mp:process-lock))
  (release-lock lock)
  (mp:process-wait "wait for message"
                   #'mp:gate-open-p
                   condition-variable)
  (acquire-lock lock)
  (mp:close-gate condition-variable))

(defmethod condition-notify ((condition-variable vector))
  (mp:open-gate condition-variable))

(defmethod thread-yield ()
  (mp:process-allow-schedule))

;;; Introspection/debugging

(defmethod all-threads ()
  mp:*all-processes*)

(defmethod interrupt-thread ((thread mp:process) function)
  (mp:process-interrupt thread function))

(defmethod destroy-thread ((thread mp:process))
  (mp:process-kill thread))

(mark-supported)
) ; end PROGN
