;;;; -*- indent-tabs-mode: nil -*-

(in-package :bordeaux-threads-2)

;;;
;;; Portable condition variables using semaphores.
;;;
;;; The implementation is meant to be correct and readable,
;;; without trying too hard to be very fast.
;;;

(defstruct (condition-variable
            (:constructor %make-condition-variable (name)))
  name
  (queue (make-queue)))

(defmethod print-object ((cv condition-variable) stream)
  (print-unreadable-object (cv stream :type t :identity t)
    (format stream "~S" (condition-variable-name cv))))

(defun %condition-wait (cv lock timeout)
  (with-slots (queue) cv
    (let* ((thread (current-thread))
           (semaphore (%thread-semaphore thread)))
      (queue-enqueue queue thread)
      (%release-lock lock)
      (unwind-protect
           (%wait-on-semaphore semaphore timeout)
        (%acquire-lock lock t nil)))))

(defun %condition-notify (cv)
  (with-slots (queue) cv
    (when-let ((next-thread (queue-dequeue queue)))
      (%signal-semaphore (%thread-semaphore next-thread) 1))))

(defun %condition-broadcast (cv)
  (with-slots (queue) cv
    (let ((queued-threads (queue-drain queue)))
      (map nil (lambda (thr)
                 (%signal-semaphore (%thread-semaphore thr) 1))
           queued-threads))))
