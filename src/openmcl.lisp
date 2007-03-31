#|
Copyright 2006,2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

;;; documentation on the OpenMCL Threads interface can be found at
;;; http://openmcl.clozure.com/Doc/Programming-with-Threads.html

#+openmcl-native-threads (progn

;;; Thread Creation
  
(defmethod make-thread (function &key name)
  (ccl:process-run-function name function))

(defmethod current-thread ()
  ccl:*current-process*)

(defmethod threadp ((object ccl:process))
  t)

(defmethod thread-name ((thread ccl:process))
  (ccl:process-name thread))

;;; Resource contention: locks and recursive locks


(defmethod make-lock (&optional name)
  (ccl:make-lock name))

(defmethod acquire-lock ((lock ccl:lock) &optional (wait-p t))
  (if wait-p
      (ccl:grab-lock lock)
      (ccl:try-lock lock)))

(defmethod release-lock ((lock ccl:lock))
  (ccl:release-lock lock))

(defmacro with-lock-held ((place) &body body)
  `(ccl:with-lock-grabbed (,place)
     ,@body))

(defmethod make-recursive-lock (&optional name)
  (ccl:make-lock name))

(defmethod acquire-recursive-lock ((lock ccl::recursive-lock))
  (ccl:grab-lock lock))

(defmethod release-recursive-lock ((lock ccl::recursive-lock))
  (ccl:release-lock lock))

(defmacro with-recursive-lock-held ((place) &body body)
  `(ccl:with-lock-grabbed (,place)
     ,@body))

;;; Resource contention: condition variables


(defmethod make-condition-variable ()
  (ccl:make-semaphore))

(defmethod condition-wait ((condition-variable ccl:semaphore) (lock ccl:lock))
  (unwind-protect
       (progn
	 (release-lock lock)
	 (ccl:wait-on-semaphore condition-variable))
    (acquire-lock lock t)))

(defmethod condition-notify ((condition-variable ccl:semaphore))
  (ccl:signal-semaphore condition-variable))

(defmethod thread-yield ()
  (ccl:process-allow-schedule))

;;; Introspection/debugging


(defmethod all-threads ()
  (ccl:all-processes))

(defmethod interrupt-thread ((thread ccl:process) function)
  (ccl:process-interrupt thread function))

(defmethod destroy-thread ((thread ccl:process))
  (ccl:process-kill thread))

(mark-supported)
) ; end PROGN