;;;; -*- Mode: LISP; Syntax: ANSI-Common-lisp; Base: 10; Package: BORDEAUX-THREADS -*-
;;;; The above modeline is required for Genera. Do not change.

(in-package :bordeaux-threads-2)

(defclass thread ()
  ((name :initarg :name :reader thread-name)
   (native-thread :initarg :native-thread
                  :reader thread-native-thread)
   (%init-lock :initform (make-lock))
   #+ccl
   (%semaphore :initform (%make-semaphore nil 0)
               :reader %thread-semaphore)
   (%return-values :initform nil :reader thread-return-values)
   (%exit-condition :initform nil :reader thread-exit-condition)))

(defmethod print-object ((thread thread) stream)
  (print-unreadable-object (thread stream :type t :identity t)
    (format stream "~S" (thread-name thread))))

(define-global-var* .known-threads.
  (trivial-garbage:make-weak-hash-table :weakness :key))

(define-global-var* .known-threads-lock.
  #+thread-support
  (make-lock :name "known-threads-lock")
  #-thread-support
  nil)

(defun thread-wrapper (&optional (native-thread (%current-thread)))
  (with-lock-held (.known-threads-lock.)
    (multiple-value-bind (thread presentp)
        (gethash native-thread .known-threads.)
      (if presentp
          thread
          (setf (gethash native-thread .known-threads.)
                (make-instance 'thread
                               :name (%thread-name native-thread)
                               :native-thread native-thread))))))

(defun (setf thread-wrapper) (thread native-thread)
  (with-lock-held (.known-threads-lock.)
    (setf (gethash native-thread .known-threads.) thread)))

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

(macrolet
    ((defbindings (name docstring &body initforms)
         (check-type docstring string)
       `(alexandria:define-constant ,name
            (list
             ,@(loop for (special form) in initforms
                     collect `(cons ',special ',form)))
          :test #'equal
          :documentation ,docstring)))
  (defbindings +standard-io-bindings+
      "Standard bindings of printer/reader control variables as per
CL:WITH-STANDARD-IO-SYNTAX. Forms are evaluated in the calling thread."
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
    (*random-state*              (make-random-state t))
    (*read-base*                 10)
    (*read-default-float-format* 'double-float)
    (*read-eval*                 nil)
    (*read-suppress*             nil)
    (*readtable*                 (copy-readtable nil))))

(defvar *current-thread*)

(defun compute-special-bindings (bindings)
  (remove-duplicates (acons '*current-thread* nil
                            (append bindings +standard-io-bindings+))
                     :from-end t :key #'car))

(defun establish-dynamic-env (thread function special-bindings trap-conditions)
  "Return a closure that binds the symbols in SPECIAL-BINDINGS and calls
FUNCTION."
  (let* ((bindings (compute-special-bindings special-bindings))
         (specials (mapcar #'car bindings))
         (values (mapcar (lambda (f) (eval (cdr f))) bindings)))
    (named-lambda %establish-dynamic-env-wrapper ()
      (progv specials values
        (with-slots (%init-lock %return-values %exit-condition) thread
          (flet ((record-condition (c)
                   (setf %exit-condition c))
                 (run-function ()
                   (with-lock-held (%init-lock)
                     (setf *current-thread*
                           (thread-wrapper (%current-thread)))
                     (setf %return-values (multiple-value-list
                                           (funcall function))))))
            (if trap-conditions
                (handler-case
                    (progn
                      (run-function)
                      (values-list %return-values))
                  (condition (c)
                    (record-condition c)))
                (handler-bind
                    ((condition #'record-condition))
                  (run-function)))))))))


;;;
;;; Thread Creation
;;;

(defun start-multiprocessing ()
  "If the host implementation uses user-level threads, start the
scheduler and multiprocessing, otherwise do nothing.
It is safe to call repeatedly."
  (when (fboundp '%start-multiprocessing)
    (funcall '%start-multiprocessing))
  (values))

(defun make-thread (function
                    &key
                      name
                      (initial-bindings *default-special-bindings*)
                      (handle-conditions t))
  "Creates and returns a thread named NAME, which will call the
  function FUNCTION with no arguments: when FUNCTION returns, the
  thread terminates.

  The interaction between threads and dynamic variables is in some
  cases complex, and depends on whether the variable has only a global
  binding (as established by e.g. DEFVAR/DEFPARAMETER/top-level SETQ)
  or has been bound locally (e.g. with LET or LET*) in the calling
  thread.

  - Global bindings are shared between threads: the initial value of a
    global variable in the new thread will be the same as in the
    parent, and an assignment to such a variable in any thread will be
    visible to all threads in which the global binding is visible.

  - Local bindings, such as the ones introduced by INITIAL-BINDINGS,
    are local to the thread they are introduced in, except that

  - Local bindings in the the caller of MAKE-THREAD may or may not be
    shared with the new thread that it creates: this is
    implementation-defined. Portable code should not depend on
    particular behaviour in this case, nor should it assign to such
    variables without first rebinding them in the new thread."
  (check-type function (and (not null) (or symbol function)))
  (check-type name (or null string))
  (let ((thread (make-instance 'thread :name name)))
    (with-slots (native-thread %init-lock) thread
        (with-lock-held (%init-lock)
          (let ((%thread
                  (%make-thread (establish-dynamic-env
                                 thread
                                 function
                                 initial-bindings
                                 handle-conditions)
                                name)))
            (setf native-thread %thread)
            (setf (thread-wrapper %thread) thread))))
    thread))

(defun current-thread ()
  "Returns the thread object for the calling thread.
  This is the same kind of object as would be returned
  by MAKE-THREAD."
  (cond
    ((boundp '*current-thread*)
     (assert (threadp *current-thread*))
     *current-thread*)
    (t (thread-wrapper (%current-thread)))))

(defun threadp (object)
  "Returns T if object is a thread, otherwise NIL."
  (typep object 'thread))

(defmethod join-thread ((thread thread))
  "Wait until THREAD terminates. If THREAD has already terminated,
  return immediately. The return values of the thread function are
  returned."
  (with-slots (native-thread %return-values %exit-condition)
      thread
    (when (eql native-thread (%current-thread))
      (bt-error "Cannot join with the current thread"))
    (%join-thread native-thread)
    (if %exit-condition
        (error 'abnormal-exit :condition %exit-condition)
        (values-list %return-values))))

(defun thread-yield ()
  "Allows other threads to run. It may be necessary or desirable to
  call this periodically in some implementations; others may schedule
  threads automatically."
  (%thread-yield)
  (values))

;;;
;;; Introspection/debugging
;;;

(defun all-threads ()
  "Returns a sequence of all of the threads. This may not
  be freshly-allocated, so the caller should not modify it."
  (mapcar #'thread-wrapper (%all-threads)))

(defmethod interrupt-thread ((thread thread) function &rest args)
  "Interrupt THREAD and cause it to evaluate FUNCTION
  before continuing with the interrupted path of execution. This may
  not be a good idea if THREAD is holding locks or doing anything
  important."
  (flet ((apply-function ()
           (if args
               (named-lambda %interrupt-thread-wrapper ()
                 (apply function args))
               function)))
    (declare (dynamic-extent #'apply-function))
    (%interrupt-thread (thread-native-thread thread) (apply-function))
    thread))

(defmethod signal-in-thread ((thread thread) datum &rest args)
  "Interrupt THREAD and call SIGNAL passing DATUM and ARGS."
  (apply #'interrupt-thread thread #'signal (cons datum args)))

(defmethod warn-in-thread ((thread thread) datum &rest args)
  "Interrupt THREAD and call WARN passing DATUM and ARGS."
  (apply #'interrupt-thread thread #'warn (cons datum args)))

(defmethod error-in-thread ((thread thread) datum &rest args)
  "Interrupt THREAD and call ERROR passing DATUM and ARGS."
  (apply #'interrupt-thread thread #'error (cons datum args)))

(defmethod destroy-thread ((thread thread))
  "Terminates the thread THREAD, which is an object
  as returned by MAKE-THREAD. This should be used with caution: it is
  implementation-defined whether the thread runs cleanup forms or
  releases its locks first.

  Destroying the calling thread is an error."
  (when (eql (thread-native-thread thread) (%current-thread))
    (bt-error "Cannot destroy the current thread"))
  (%destroy-thread (thread-native-thread thread))
  thread)

(defmethod thread-alive-p ((thread thread))
  "Returns true if THREAD is alive, that is, if it has not finished or
  DESTROY-THREAD has not been called on it."
  (%thread-alive-p (thread-native-thread thread)))
