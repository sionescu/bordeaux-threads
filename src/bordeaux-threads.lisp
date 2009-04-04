;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(defpackage bordeaux-threads
  (:nicknames #:bt)
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
           #:*default-special-bindings* #:*standard-io-bindings*
           #:*supports-threads-p*

           #:make-lock #:acquire-lock #:release-lock #:with-lock-held
           #:make-recursive-lock #:acquire-recursive-lock
           #:release-recursive-lock #:with-recursive-lock-held

           #:make-condition-variable #:condition-wait #:condition-notify
           #:thread-yield

           #:with-timeout #:timeout

           #:all-threads #:interrupt-thread #:destroy-thread #:thread-alive-p
           #:join-thread))

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

(define-condition timeout (serious-condition) ())

;;; Thread Creation

;;; See default-implementations.lisp for MAKE-THREAD.

;; Forms are evaluated in the new thread or in the calling thread?
(defvar *default-special-bindings* nil
  "This variable holds an alist associating special variable symbols
  to forms to evaluate. Special variables named in this list will
  be locally bound in the new thread before it begins executing user code.

  This variable may be rebound around calls to MAKE-THREAD to
  add/alter default bindings. The effect of mutating this list is
  undefined, but earlier forms take precedence over later forms for
  the same symbol, so defaults may be overridden by consing to the
  head of the list.")

(defmacro defbindings (name docstring &body initforms)
  (check-type docstring string)
  `(defparameter ,name
     (list
      ,@(loop for (special form) in initforms
              collect `(cons ',special ',form)))
     ,docstring))

;; Forms are evaluated in the new thread or in the calling thread?
(defbindings *standard-io-bindings*
  "Standard bindings of printer/reader control variables as per CL:WITH-STANDARD-IO-SYNTAX."
  (*package*                   (find-package :common-lisp-user))
  (*print-array*               t)
  (*print-base*                10)
  (*print-case*                :upcase)
  (*print-circle*              nil)
  (*print-escape*              t)
  (*print-gensym*              t)
  (*print-length*              nil)
  (*print-level*               nil)
  (*print-lines*               nil)
  (*print-miser-width*         nil)
  (*print-pprint-dispatch*     (copy-pprint-dispatch nil))
  (*print-pretty*              nil)
  (*print-radix*               nil)
  (*print-readably*            t)
  (*print-right-margin*        nil)
  (*read-base*                 10)
  (*read-default-float-format* 'single-float)
  (*read-eval*                 t)
  (*read-suppress*             nil)
  (*readtable*                 (copy-readtable nil)))

(defun binding-default-specials (function special-bindings)
  "Return a closure that binds the symbols in SPECIAL-BINDINGS and calls
FUNCTION."
  (let ((specials (remove-duplicates special-bindings :from-end t :key #'car)))
    (lambda ()
      (progv (mapcar #'car specials)
          (loop for (nil . form) in specials collect (eval form))
        (funcall function)))))

;;; FIXME: This test won't work if CURRENT-THREAD
;;;        conses a new object each time
(defun signal-error-if-current-thread (thread)
  (when (eq thread (current-thread))
    (error 'bordeaux-mp-condition
           :message "Cannot destroy the current thread")))
