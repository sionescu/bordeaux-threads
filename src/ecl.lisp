#|
Copyright 2006,2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

;;; documentation on the ECL Multiprocessing interface can be found at
;;; http://ecls.sourceforge.net/cgi-bin/view/Main/MultiProcessing

;;; FIXME: Need some sort of *FEATURES* check

;;; Thread Creation

(defmethod make-thread (function &key name)
  (mp:process-run-function (or name "") function))

(defmethod current-thread ()
  mp::*current-process*)

(defmethod threadp ((object mp:process))
  t)

(defmethod thread-name ((thread mp:process))
  (mp:process-name thread))

;;; Resource contention: locks and recursive locks

(defmethod make-lock (&optional name)
  (mp:make-lock :name name))

(defmethod acquire-lock ((lock mp:lock) &optional (wait-p t))
  (mp:get-lock lock wait-p))

(defmethod release-lock ((lock mp:lock))
  (mp:giveup-lock lock))

(defmacro with-lock-held ((place) &body body)
  `(mp:with-lock (,place) ,@body))

;; make-recursive-lock
;; acquire-recursive-lock
;; release-recursive-lock

;;; Resource contention: condition variables

(defmethod thread-yield ()
  ;; (mp:yield)
  (sleep 0))

;;; Introspection/debugging

(defmethod all-threads ()
  (mp:all-processes))

(defmethod interrupt-thread ((thread mp:process) function)
  (mp:interrupt-process thread function))

(defmethod destroy-thread ((thread mp:process))
  (mp:process-kill thread))

(mark-supported)
