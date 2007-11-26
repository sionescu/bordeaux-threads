#|
Copyright 2006,2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(defpackage bordeaux-threads
  (:nicknames #:bt #:threads)
  (:documentation "BORDEAUX-THREADS is a proposed standard for a minimal
  MP/threading interface. It is similar to the CLIM-SYS threading and
  lock support, but for the following broad differences:

  1) Some behaviours are defined in additional detail: attention has
     been given to special variable interaction, whether and when
     cleanup forms are run. Some behaviours are defined in less
     detail: an implementation that does not support multiple
     threads is not required to use a new list (nil) for a lock, for
     example.

  2) Many functions which would be difficult, dangerous or inefficient
     to provide on some implementations have been removed. Chiefly
     these are functions such as thread-wait which expect for
     efficiency that the thread scheduler is written in Lisp and
     'hookable', which can't sensibly be done if the scheduler is
     external to the Lisp image, or the system has more than one CPU.

  3) Unbalanced ACQUIRE-LOCK and RELEASE-LOCK functions have been
     added.

  4) Posix-style condition variables have been added, as it's not
     otherwise possible to implement them correctly using the other
     operations that are specified.

  Threads may be implemented using whatever applicable techniques are
  provided by the operating system: user-space scheduling,
  kernel-based LWPs or anything else that does the job.

  Some parts of this specification can also be implemented in a Lisp
  that does not support multiple threads. Thread creation and some
  thread inspection operations will not work, but the locking
  functions are still present (though they may do nothing) so that
  thread-safe code can be compiled on both multithread and
  single-thread implementations without need of conditionals.

  To avoid conflict with existing MP/threading interfaces in
  implementations, these symbols live in the BORDEAUX-THREADS package.
  Implementations and/or users may also make them visible or exported
  in other more traditionally named packages.")
  (:use #:cl)
  (:export #:make-thread #:current-thread #:threadp #:thread-name
	   #:*default-special-bindings* #:*supports-threads-p*

	   #:make-lock #:acquire-lock #:release-lock #:with-lock-held
	   #:make-recursive-lock #:acquire-recursive-lock
	   #:release-recursive-lock #:with-recursive-lock-held

	   #:make-condition-variable #:condition-wait #:condition-notify
	   #:thread-yield

	   #:all-threads #:interrupt-thread #:destroy-thread #:thread-alive-p))

(in-package #:bordeaux-threads)

(defvar *supports-threads-p* nil
  "This should be set to T if the running instance has thread support.")

(defun mark-supported ()
  (setf *supports-threads-p* t)
  (pushnew :bordeaux-threads *features*))

(define-condition bordeaux-mp-condition (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
	     (format stream (message condition)))))

(defgeneric make-threading-support-error ()
  (:documentation "Creates a BORDEAUX-THREADS condition which specifies
  whether there is no BORDEAUX-THREADS support for the implementation, no
  threads enabled for the system, or no support for a particular
  function.")
  (:method ()
    (make-condition
     'bordeaux-mp-condition
     :message (if *supports-threads-p*
                  "There is no support for this method on this implementation."
                  "There is no thread support in this instance."))))

;;; Thread Creation

;;; See default-implementations.lisp for MAKE-THREAD.

(defvar *default-special-bindings* '()
  "This variable holds an alist associating special variable symbols
  with forms to evaluate for binding values. Special variables named
  in this list will be locally bound in the new thread before it
  begins executing user code.

  This variable may be rebound around calls to MAKE-THREAD to
  add/alter default bindings. The effect of mutating this list is
  undefined, but earlier forms take precedence over later forms for
  the same symbol, so defaults may be overridden by consing to the
  head of the list."
  ;; Forms are evaluated in the new thread or in the calling thread?
  ;; Standard contents of this list: print/reader control, etc. Can
  ;; borrow the franz equivalent?
  )

(defgeneric current-thread ()
  (:documentation "Returns the thread object for the calling
  thread. This is the same kind of object as would be returned by
  MAKE-THREAD."))

(defgeneric threadp (object)
  (:documentation "Returns true if object is a thread, otherwise NIL.")
  (:method (object)
    (declare (ignore object))
    nil))

(defgeneric thread-name (thread)
  (:documentation "Returns the name of the thread, as supplied to
  MAKE-THREAD"))

;;; Resource contention: locks and recursive locks

;;; See default-implementations.lisp for MAKE-LOCK, MAKE-RECURSIVE-LOCK, WITH-LOCK-HELD, and WITH-RECURSIVE-LOCK-HELD.

(defgeneric acquire-lock (lock &optional wait-p)
  (:documentation "Acquire the lock LOCK for the calling thread.
  WAIT-P governs what happens if the lock is not available: if WAIT-P
  is true, the calling thread will wait until the lock is available
  and then acquire it; if WAIT-P is NIL, ACQUIRE-LOCK will return
  immediately. ACQUIRE-LOCK returns true if the lock was acquired and
  NIL otherwise.

  This specification does not define what happens if a thread
  attempts to acquire a lock that it already holds. For applications
  that require locks to be safe when acquired recursively, see instead
  MAKE-RECURSIVE-LOCK and friends.")
  (:method (lock &optional wait-p)
    (declare (ignore lock wait-p))
    t))

(defgeneric release-lock (lock)
  (:documentation "Release LOCK. It is an error to call this unless
  the lock has previously been acquired (and not released) by the same
  thread. If other threads are waiting for the lock, the
  ACQUIRE-LOCK call in one of them will now be able to continue.

  This function has no interesting return value.")
  (:method (lock)
    (declare (ignore lock))
    (values)))

(defgeneric acquire-recursive-lock (lock)
  (:documentation "As for ACQUIRE-LOCK, but for recursive locks.")
  (:method (lock)
    (declare (ignore lock))
    t))

(defgeneric release-recursive-lock (lock)
  (:documentation "Release the recursive LOCK. The lock will only
  become free after as many Release operations as there have been
  Acquire operations. See RELEASE-LOCK for other information.")
  (:method (lock)
    (declare (ignore lock))
    (values)))

;;; Resource contention: condition variables

;;; A condition variable provides a mechanism for threads to put
;;; themselves to sleep while waiting for the state of something to
;;; change, then to be subsequently woken by another thread which has
;;; changed the state.
;;;
;;; A condition variable must be used in conjunction with a lock to
;;; protect access to the state of the object of interest. The
;;; procedure is as follows:
;;;
;;; Suppose two threads A and B, and some kind of notional event
;;; channel C. A is consuming events in C, and B is producing them.
;;; CV is a condition-variable
;;;
;;; 1) A acquires the lock that safeguards access to C
;;; 2) A threads and removes all events that are available in C
;;; 3) When C is empty, A calls CONDITION-WAIT, which atomically
;;;    releases the lock and puts A to sleep on CV
;;; 4) Wait to be notified; CONDITION-WAIT will acquire the lock again
;;;    before returning
;;; 5) Loop back to step 2, for as long as threading should continue
;;;
;;; When B generates an event E, it
;;; 1) acquires the lock guarding C
;;; 2) adds E to the channel
;;; 3) calls CONDITION-NOTIFY on CV to wake any sleeping thread
;;; 4) releases the lock
;;;
;;; To avoid the "lost wakeup" problem, the implementation must
;;; guarantee that CONDITION-WAIT in thread A atomically releases the
;;; lock and sleeps. If this is not guaranteed there is the
;;; possibility that thread B can add an event and call
;;; CONDITION-NOTIFY between the lock release and the sleep - in this
;;; case the notify call would not see A, which would be left sleeping
;;; despite there being an event available.

;;; see default-implementations.lisp for THREAD-YIELD.

(defgeneric make-condition-variable ()
  (:documentation "Returns a new condition-variable object for use
  with CONDITION-WAIT and CONDITION-NOTIFY."))

(defgeneric condition-wait (condition-variable lock)
  (:documentation "Atomically release LOCK and enqueue the calling
  thread waiting for CONDITION-VARIABLE. The thread will resume when
  another thread has notified it using CONDITION-NOTIFY; it may also
  resume if interrupted by some external event or in other
  implementation-dependent circumstances: the caller must always test
  on waking that there is threading to be done, instead of assuming
  that it can go ahead.

  However and for whatever reason the thread is resumed, the system
  always reacquires LOCK before returning to the caller. It is an
  error to call this unless from the thread that holds LOCK.

  In an implementation that does not support multiple threads, this
  function signals an error.")
  (:method (condition-variable lock)
    (declare (ignore condition-variable lock))
    (error (make-threading-support-error))))

(defgeneric condition-notify (condition-variable)
  (:documentation "Notify at least one of the threads waiting for
  CONDITION-VARIABLE. It is implementation-dependent whether one or
  more than one (and possibly all) threads are woken, but if the
  implementation is capable of waking only a single thread (not all
  are) this is probably preferable for efficiency reasons. The order
  of wakeup is unspecified and does not necessarily relate to the
  order that the threads went to sleep in.

  CONDITION-NOTIFY has no useful return value. In an implementation
  that does not support multiple threads, it has no effect.")
  (:method (condition-variable)
    (declare (ignore condition-variable))
    (values)))

;;; Introspection/debugging

;;; The following functions may be provided for debugging purposes,
;;; but are not advised to be called from normal user code.

(defgeneric all-threads ()
  (:documentation "Returns a sequence of all of the threads. This
  may or may not be freshly-allocated, so the caller should not modify
  it."))

(defgeneric interrupt-thread (thread function)
  (:documentation "Interrupt THREAD and cause it to evaluate FUNCTION
  before continuing with the interrupted path of execution. This may
  not be a good idea if THREAD is holding locks or doing anything
  important. On systems that do not support multiple threads, this
  function signals an error.")
  (:method (thread function)
    (declare (ignore thread function))
    (error (make-threading-support-error))))

(defgeneric destroy-thread (thread)
  (:documentation "Terminates the thread THREAD, which is an object
  as returned by MAKE-THREAD. This should be used with caution: it is
  implementation-defined whether the thread runs cleanup forms or
  releases its locks first.

  Destroying the calling thread is an error.")
  (:method :before (thread)
	   (when (eq thread (current-thread))
	     (error
	      (make-condition 'bordeaux-mp-condition
			      :message "Can not destroy the current thread")))))

(defgeneric thread-alive-p (thread)
  (:documentation "Returns true if THREAD is alive, that is, if
  DESTROY-THREAD has not been called on it.")
  (:method (thread)
    (declare (ignore thread))
    (error (make-threading-support-error))))
