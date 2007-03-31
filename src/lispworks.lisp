#|
Copyright 2006,2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

;;; documentation on the LispWorks Multiprocessing interface can be found at
;;; http://www.lispworks.com/documentation/lw445/LWUG/html/lwuser-156.htm

(mp:initialize-multiprocessing)

;;; Thread Creation

(defmethod make-thread (function &key name)
  (mp:process-run-function name nil function))

(defmethod current-thread ()
  mp:*current-process*)

(defmethod threadp ((object mp:process))
  t)

(defmethod thread-name ((thread mp:process))
  (mp:process-name thread))

;;; Resource contention: locks and recursive locks

(defmethod make-lock (&optional name)
  (mp:make-lock :name name))

(defmethod acquire-lock ((lock mp:lock) &optional (wait-p t))
  (mp:process-lock lock nil (if wait-p
                                (if (typep wait-p 'number) wait-p nil)
                                0)))

(defmethod release-lock ((lock mp:lock))
  (mp:process-unlock lock))

;;; Apparently this EVAL-WHEN is needed so that the macro is available
;;; when compiling condition-variables.lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-lock-held ((place) &body body)
    `(mp:with-lock (,place) ,@body)))

;;; Resource contention: condition variables

(defmethod thread-yield ()
  (mp:process-allow-scheduling))

;;; Introspection/debugging

(defmethod all-threads ()
  (mp:list-all-processes))

(defmethod interrupt-thread ((thread mp:process) function)
  (mp:process-interrupt thread function))

(defmethod destroy-process ((thread mp:process))
  (mp:process-kill thread))

(mark-supported)
