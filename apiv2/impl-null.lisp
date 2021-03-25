(in-package :bordeaux-threads-2)

;;;
;;; Threads
;;;

(defun %make-thread (function name)
  (declare (ignore function name))
  (signal-not-implemented 'make-thread))

(defun %current-thread ()
  (signal-not-implemented 'current-thread))

(defun %thread-name (thread)
  (declare (ignore thread))
  (signal-not-implemented 'thread-name))

(defun %join-thread (thread)
  (declare (ignore thread))
  (signal-not-implemented 'join-thread))

(defun %thread-yield ()
  (signal-not-implemented 'thread-yield))

;;;
;;; Introspection/debugging
;;;

;;; VTZ: mt:list-threads returns all threads that are not garbage collected.
(defun %all-threads ()
  (signal-not-implemented 'all-threads))

(defun %interrupt-thread (thread function)
  (declare (ignore thread function))
  (signal-not-implemented 'interrupt-thread))

(defun %destroy-thread (thread)
  (declare (ignore thread))
  (signal-not-implemented 'destroy-thread))

(defun %thread-alive-p (thread)
  (declare (ignore thread))
  (signal-not-implemented 'thread-alive-p))


;;;
;;; Non-recursive locks
;;;


(defun %make-lock (name)
  (declare (ignore name))
  (signal-not-implemented 'make-lock))

(defun %acquire-lock (lock waitp timeout)
  (declare (ignore lock waitp timeout))
  (signal-not-implemented 'acquire-lock))

(defun %release-lock (lock)
  (declare (ignore lock))
  (signal-not-implemented 'release-lock))

(defvar *with-lock-warned* nil)

(defmacro %with-lock ((place timeout) &body body)
  (declare (ignore place timeout))
  (unless *with-lock-warned*
    (setf *with-lock-warned* t)
    (warn "No threading support, WITH-LOCK will be replaced with PROGN"))
  `(progn ,@body))

;;;
;;; Recursive locks
;;;


(defun %make-recursive-lock (name)
  (declare (ignore name))
  (signal-not-implemented 'make-recursive-lock))

(defun %acquire-recursive-lock (lock waitp timeout)
  (declare (ignore lock waitp timeout))
  (signal-not-implemented 'acquire-recursive-lock))

(defun %release-recursive-lock (lock)
  (declare (ignore lock))
  (signal-not-implemented 'release-recursive-lock))

(defvar *with-recursive-lock-warned* nil)

(defmacro %with-recursive-lock ((place timeout) &body body)
  (declare (ignore place timeout))
  (unless *with-recursive-lock-warned*
    (setf *with-recursive-lock-warned* t)
    (warn "No threading support, WITH-RECURSIVELOCK will be replaced with PROGN"))
  `(progn ,@body))


;;;
;;; Condition variables
;;;


(defun %make-condition-variable (name)
  (declare (ignore name))
  (signal-not-implemented 'make-condition-variable))

(defun %condition-wait (cv lock timeout)
  (declare (ignore cv lock timeout))
  (signal-not-implemented 'condition-wait))

(defun %condition-notify (cv)
  (declare (ignore cv))
  (signal-not-implemented 'conditioin-notify))

(defun %condition-broadcast (cv)
  (declare (ignore cv))
  (signal-not-implemented 'condition-broadcast))


;;;
;;; Timeouts
;;;

(defvar *with-timeout-warned* nil)

(defmacro with-timeout ((timeout) &body body)
  (declare (ignore timeout))
  (unless *with-timeout-warned*
    (setf *with-timeout-warned* t)
    (warn "No threading support, WITH-TIMEOUT will be replaced with PROGN"))
  `(progn ,@body))
