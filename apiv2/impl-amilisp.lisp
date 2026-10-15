;;;; -*- indent-tabs-mode: nil -*-

;;;; AmiLisp backend for bordeaux-threads API v2.
;;;; Uses the MP package provided by AmiLisp's threading primitives.

(in-package :bordeaux-threads-2)

;;; AmiLisp is always 32-bit (CL_Obj = uint32_t).
;;; Ensure :32-BIT is in *features* for atomics.lisp.
(eval-when (:compile-toplevel :load-toplevel :execute)
  #-(or 32-bit 64-bit)
  (pushnew :32-bit *features*))

;;;
;;; Threads
;;;

(deftype native-thread ()
  'mp:thread)

(defun %make-thread (function name)
  (mp:make-thread function :name name))

(defun %current-thread ()
  (mp:current-thread))

(defun %thread-name (thread)
  (let ((name (mp:thread-name thread)))
    (if name (string name) nil)))

(defun %join-thread (thread)
  (mp:join-thread thread))

(defun %thread-yield ()
  (mp:thread-yield))

;;;
;;; Introspection/debugging
;;;

(defun %all-threads ()
  (mp:all-threads))

(defun %interrupt-thread (thread function)
  (mp:interrupt-thread thread function))

(defun %destroy-thread (thread)
  (mp:destroy-thread thread))

(defun %thread-alive-p (thread)
  (mp:thread-alive-p thread))


;;;
;;; Non-recursive locks
;;;

(deftype native-lock () 'mp:lock)

(defun %make-lock (name)
  (mp:make-lock name))

;; MP:ACQUIRE-LOCK takes the timeout (seconds, or NIL for "forever") as its
;; third argument and returns NIL when it elapses; it is only consulted
;; when WAITP is true, which is the API's contract too.
(defun %acquire-lock (lock waitp timeout)
  (mp:acquire-lock lock waitp timeout))

(defun %release-lock (lock)
  (mp:release-lock lock))

(defmacro %with-lock ((place timeout) &body body)
  `(mp:with-lock-held (,place :timeout ,timeout) ,@body))


;;;
;;; Recursive locks
;;; MP:MAKE-RECURSIVE-LOCK returns a genuinely re-entrant lock.
;;; Re-entrancy is a property of the lock object, not
;;; of the acquire call, so MP:ACQUIRE-LOCK and MP:WITH-LOCK-HELD are the
;;; correct primitives to use on one.
;;;

(deftype native-recursive-lock () 'mp:lock)

(defun %make-recursive-lock (name)
  (mp:make-recursive-lock name))

(defun %acquire-recursive-lock (lock waitp timeout)
  (mp:acquire-lock lock waitp timeout))

(defun %release-recursive-lock (lock)
  (mp:release-lock lock))

(defmacro %with-recursive-lock ((place timeout) &body body)
  `(%with-lock (,place ,timeout) ,@body))


;;;
;;; Condition variables
;;;

(deftype condition-variable ()
  'mp:condition-variable)

(defun %make-condition-variable (name)
  (mp:make-condition-variable name))

(defun %condition-wait (cv lock timeout)
  (if timeout
      (mp:condition-wait cv lock timeout)
      (mp:condition-wait cv lock)))

(defun %condition-notify (cv)
  (mp:condition-notify cv))

(defun %condition-broadcast (cv)
  (mp:condition-broadcast cv))
