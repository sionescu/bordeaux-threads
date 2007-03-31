#|
Copyright 2006,2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

;;; Thread Creation

(defmethod make-thread (function &key name)
  (ccl:process-run-function name function))

(defmethod current-thread ()
  ccl:*current-thread*)

(defmethod threadp (object)
  (ccl:processp object))

(defmethod thread-name (thread)
  (ccl:process-name thread))

;;; Resource contention: locks and recursive locks

(defmethod make-lock (&optional name)
  (ccl:make-lock name))

(defmacro with-lock-held ((place) &body body)
  `(ccl:with-lock-grabbed (,place) ,@body))

(defmethod thread-yield ()
  (ccl:process-allow-schedule))

;;; Introspection/debugging

(defmethod all-threadss ()
  ccl:*all-processes*)

(defmethod interrupt-thread (thread function)
  (ccl:process-interrupt thread function))

(defmethod destroy-thread (thread)
  (ccl:process-kill thread))

(mark-supported)
