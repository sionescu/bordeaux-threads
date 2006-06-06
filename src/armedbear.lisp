(in-package #:bordeaux-threads)

(setf *supports-threads-p* t)

;;; Thread Creation

(defmethod make-thread (function &key name)
  (ext:make-thread function :name name))

(defmethod current-thread ()
  (ext:current-thread))

(defmethod thread-name (thread)
  (ext:thread-name thread))

;;; Resource contention: locks and recursive locks

(defmethod make-lock (&optional name)
  (ext:make-thread-lock name))

(defmethod acquire-lock (lock &optional (wait-p t))
  (ext:thread-lock lock))

(defmethod release-lock (lock)
  (ext:thread-unlock lock))

(defmacro with-lock-held ((place) &body body)
  `(ext:with-thread-lock (,place) ,@body))

;;; Introspection/debugging

(defmethod interrupt-thread (thread function)
  (ext:interrupt-thread thread function))

(defmethod destroy-thread (thread)
  (ext:destroy-thread thread))
