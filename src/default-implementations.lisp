(in-package #:bordeaux-threads)

(defmacro defdfun (name args doc &body body)
  `(progn
     (when (not (fboundp ',name))
       (defun ,name ,args ,@body))
     (setf (documentation ',name 'function)
           (format nil "~a~@[~%~%~a~]"
                   ,doc (documentation ',name 'function)))))

(defmacro defdmacro (name args doc &body body)
  `(progn
     (when (not (fboundp ',name))
       (defmacro ,name ,args ,@body))
     (setf (documentation ',name 'function)
           (format nil "~a~@[~%~%~a~]"
                   ,doc (documentation ',name 'function)))))

(defdfun make-thread (function &key name)
  "Creates and returns a thread named NAME, which will call the
  function FUNCTION with no arguments: when FUNCTION returns, the
  thread terminates. NAME defaults to NIL if unsupplied.

  On systems that do not support multi-threading, MAKE-THREAD will
  signal an error.

  The interaction between threads and dynamic variables is in some
  cases complex, and depends on whether the variable has only a global
  binding (as established by e.g. DEFVAR/DEFPARAMETER/top-level SETQ)
  or has been bound locally (e.g. with LET or LET*) in the calling
  thread.

  - Global bindings are shared between threads: the initial value of a
    global variable in the new thread will be the same as in the
    parent, and an assignment to such a variable in any thread will be
    visible to all threads in which the global binding is visible.

  - Local bindings are local to the thread they are introduced in,
    except that

  - Local bindings in the the caller of MAKE-THREAD may or may not be
    shared with the new thread that it creates: this is
    implementation-defined. Portable code should not depend on
    particular behaviour in this case, nor should it assign to such
    variables without first rebinding them in the new thread."
  (declare (ignore function name))
  (error (make-mp-support-error)))

(defdfun make-lock (&optional name)
  "Creates a lock (a mutex) whose name is NAME. If the system does not
  support multiple threads this will still return some object, but it
  may not be used for very much."
  ;; In CLIM-SYS this is a freshly consed list (NIL). I don't know if
  ;; there's some good reason it should be said structure or that it
  ;; be freshly consed - EQ comparison of locks?
  (declare (ignore name))
  (list nil))

(defdfun make-recursive-lock (&optional name)
  "Create and return a recursive lock whose name is NAME. A recursive
  lock differs from an ordinary lock in that a thread that already
  holds the recursive lock can acquire it again without blocking. The
  thread must then release the lock twice before it becomes available
  for another thread."
  (declare (ignore name))
  (list nil))

(defdmacro with-lock-held ((place) &body body)
    "Evaluates BODY with the lock named by PLACE, the value of which
is a lock created by MAKE-LOCK. Before the forms in BODY are
evaluated, the lock is acquired as if by using ACQUIRE-LOCK. After the
forms in BODY have been evaluated, or if a non-local control transfer
is caused (e.g. by THROW or SIGNAL), the lock is released as if by
RELEASE-LOCK.

Note that if the debugger is entered, it is unspecified whether the
lock is released at debugger entry or at debugger exit when execution
is restarted."
    `(when (acquire-lock ,place t)
       (unwind-protect
            (locally ,@body)
         (release-lock ,place))))

(defdmacro with-recursive-lock-held ((place &key timeout) &body body)
  "Evaluates BODY with the recursive lock named by PLACE, which is a
reference to a recursive lock created by MAKE-RECURSIVE-LOCK. See
WITH-LOCK-HELD etc etc"
  (declare (ignore timeout))
  `(when (acquire-recursive-lock ,place t)
     (unwind-protect
          (locally ,@body)
       (release-recursive-lock ,place))))

(defdfun thread-yield ()
  "Allows other threads to run. It may be necessary or desirable to
  call this periodically in some implementations; others may schedule
  threads automatically. On systems that do not support
  multi-threading, this does nothing."
  (values))

