#|
Copyright 2006,2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

;;; documentation on the SBCL Threads interface can be found at
;;; http://www.sbcl.org/manual/Threading.html

#+sb-thread (progn

;;; Thread Creation

(defmethod make-thread (function &key name)
  (sb-thread:make-thread function :name name))

(defmethod current-thread ()
  sb-thread:*current-thread*)

(defmethod threadp ((object sb-thread:thread))
  t)

(defmethod thread-name ((thread sb-thread:thread))
  (sb-thread:thread-name thread))

;;; Resource contention: locks and recursive locks

(defmethod make-lock (&optional name)
  (sb-thread:make-mutex :name name))

(defmethod acquire-lock ((lock sb-thread:mutex) &optional (wait-p t))
  (sb-thread:get-mutex lock nil wait-p))

(defmethod release-lock ((lock sb-thread:mutex))
  (sb-thread:release-mutex lock))

(defmacro with-lock-held ((place) &body body)
  `(sb-thread:with-mutex (,place) ,@body))

(defmethod make-recursive-lock (&optional name)
  (sb-thread:make-mutex :name name))

;;; XXX acquire-recursive-lock and release-recursive-lock are actually
;;; complicated because we can't use control stack tricks.  We need to
;;; actually count something to check that the acquire/releases are
;;; balanced

(defmacro with-recursive-lock-held ((place) &body body)
  `(sb-thread:with-recursive-lock (,place)
     ,@body))

;;; Resource contention: condition variables

(defmethod make-condition-variable ()
  (sb-thread:make-waitqueue))

(defmethod condition-wait ((condition-variable sb-thread:waitqueue)
			   (lock sb-thread:mutex))
  (sb-thread:condition-wait condition-variable lock))

(defmethod condition-notify ((condition-variable sb-thread:waitqueue))
  (sb-thread:condition-notify condition-variable))

(defmethod thread-yield ()
  (sb-thread:release-foreground))

;;; Introspection/debugging

(defmethod all-threads ()
  (sb-thread:list-all-threads))

(defmethod interrupt-thread ((thread sb-thread:thread) function)
  (sb-thread:interrupt-thread thread function))

(defmethod destroy-thread ((thread sb-thread:thread))
  (sb-thread:terminate-thread thread))

(mark-supported)
) ; end PROGN
