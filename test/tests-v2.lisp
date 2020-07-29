;;;; -*- indent-tabs-mode: nil -*-

(in-package :bordeaux-threads-2/test)

(in-suite :bordeaux-threads-2)

;;;
;;; Threads
;;;

(test join-thread.return-value
  (is (eql 0 (join-thread (make-thread (lambda () 0))))))

(test current-thread.not-null
  (is (current-thread)))

(test current-thread.eql
  (is (eql (current-thread)
           (current-thread))))

#+#.(bt2::implemented-p* 'bt2:join-thread)
(test current-thread.identity
  (let ((thread (make-thread #'current-thread)))
    (is (eql thread (join-thread thread)))))

#+#.(bt2::implemented-p* 'bt2:join-thread)
(test current-thread.special
  (let ((thread (make-thread (lambda () bt2::*current-thread*))))
    (is (eql thread (join-thread thread)))))

#+#.(bt2::implemented-p* 'bt2:join-thread)
(test current-thread.error
  (let ((thread (make-thread (lambda ()
                               (error "FOOBAR")))))
    (signals abnormal-exit (join-thread thread))))

(test threadp.should-identify-threads
  (is (threadp (current-thread)))
  (is (threadp (make-thread (lambda () t) :name "foo")))
  (is (not (threadp (make-lock)))))

(test thread-name.should-retrieve-thread-name
  (is (equal "foo" (thread-name
                    (make-thread (lambda () t) :name "foo")))))

(defparameter *some-special* :global-value)

(test default-special-bindings.sees-global-bindings
  (let* ((*some-special* :local-value)
         (*default-special-bindings*
           `((*some-special* . (list :more *some-special*))
             ,@*default-special-bindings*))
         (thread (make-thread (lambda () *some-special*))))
    (is (equal (join-thread thread) '(:more :local-value)))))

(defparameter *shared* 0)
(defparameter *lock* (make-lock))

#+#.(bt2::implemented-p* 'bt2:thread-yield)
(test should-have-thread-interaction
  ;; this simple test generates N process. Each process grabs and
  ;; releases the lock until SHARED has some value, it then
  ;; increments SHARED. the outer code first sets shared 1 which
  ;; gets the thing running and then waits for SHARED to reach some
  ;; value. this should, i think, stress test locks.
  (setf *shared* 0)
  (flet ((worker (i)
           (loop
             do (with-lock-held (*lock*)
                  (when (= i *shared*)
                    (incf *shared*)
                    (return)))
                (thread-yield)
                (sleep 0.001))))
    (let* ((procs (loop
                    for i from 1 upto 2
                    ;; create a new binding to protect against implementations that
                    ;; mutate instead of binding the loop variable
                    collect (let ((i i))
                              (make-thread (lambda ()
                                             (funcall #'worker i))
                                           :name (format nil "Proc #~D" i))))))
      (with-lock-held (*lock*)
        (incf *shared*))
      (block test
        (loop
          until (with-lock-held (*lock*)
                  (= (1+ (length procs)) *shared*))
          do (with-lock-held (*lock*)
               (is (>= (1+ (length procs)) *shared*)))
             (thread-yield)
             (sleep 0.001))))))

(test all-threads.contains-threads
  (is (every #'threadp (all-threads))))

(test all-threads.contains-new-thread
  (let ((thread (make-thread (lambda () (sleep 60)))))
    (is (find thread (all-threads)))))

#+#.(bt2::implemented-p* 'bt2:interrupt-thread)
(test interrupt-thread.throw
  (let ((thread (make-thread (lambda ()
                               (catch 'new-thread
                                 (sleep 60)
                                 'not-interrupted)))))
    (sleep 1)
    (is (threadp
         (interrupt-thread thread (lambda ()
                                    (throw 'new-thread 'interrupted)))))
    (is (eql 'interrupted (join-thread thread)))))

(test thread-alive-p.new-thread
  (is (thread-alive-p (make-thread (lambda () (sleep 60))))))

#+#.(bt2::implemented-p* 'bt2:destroy-thread)
(test destroy-thread.terminates
  (let ((thread (make-thread (lambda () (sleep 3)))))
    (is (threadp (destroy-thread thread)))
    (sleep 5)
    (is-false (thread-alive-p thread))))

#+sbcl
(test destroy-thread.join-error
  (let ((thread (make-thread (lambda () (sleep 3)))))
    (destroy-thread thread)
    (signals error (join-thread thread))))


;;;
;;; Non-recursive Locks
;;;

(test lock.constructor
  (let ((lock (make-lock :name "Name")))
    (is (lockp lock))
    (is (native-lock-p (lock-native-lock lock)))
    (is (equal "Name" (lock-name lock)))))

(test acquire-lock.no-contention
  (with-fixture using-lock ()
    (is (acquire-lock lock :wait t))
    (is (lockp (release-lock lock)))
    (is (acquire-lock lock :wait nil))
    (is (lockp (release-lock lock)))))

#+#.(bt2::implemented-p* 'bt2:acquire-recursive-lock)
(test acquire-recursive-lock
  (let ((test-lock (make-recursive-lock))
        (results (make-array 4 :adjustable t :fill-pointer 0))
        (results-lock (make-lock))
        (threads ()))
    (flet ((add-result (r)
             (with-lock-held (results-lock)
               (vector-push-extend r results))))
      (dotimes (i 2)
        (push (make-thread
               #'(lambda ()
                   (when (acquire-recursive-lock test-lock)
                     (unwind-protect
                          (progn
                            (add-result :enter)
                            (sleep 1)
                            (add-result :leave))
                       (release-recursive-lock test-lock)))))
              threads)))
    (map 'nil #'join-thread threads)
    (is (equalp results #(:enter :leave :enter :leave)))))

(test acquire-lock.try-lock
  (let ((lock (make-lock)))
    (make-thread (lambda ()
                   (acquire-lock lock)
                   (sleep 60)))
    (sleep 3)
    (is-false (acquire-lock lock :wait nil))))

(test acquire-lock.timeout-expires
  (let ((lock (make-lock)))
    (make-thread (lambda ()
                   (acquire-lock lock)
                   (sleep 60)))
    (sleep 3)
    (is (null (acquire-lock lock :timeout .1)))))

(test with-lock-held.timeout-no-contention-acquired
  (let ((lock (make-lock)))
    (is (eql :ok (with-lock-held (lock :timeout .1) :ok)))))

#+#.(bt2::implemented-p* 'bt2:acquire-lock)
(test with-lock-held.timeout-expires
  (let ((lock (make-lock)))
    (make-thread (lambda ()
                   (acquire-lock lock)
                   (sleep 60)))
    (sleep 1)
    (is (eql :timeout
             (block ok
               (with-lock-held (lock :timeout .1)
                 (return-from ok :ok))
               :timeout)))))

;;;
;;; Recursive Locks
;;;

(test acquire-recursive-lock.no-contention
  (let ((lock (make-recursive-lock)))
    (is (acquire-recursive-lock lock :wait t))
    (is (recursive-lock-p (release-recursive-lock lock)))
    (is (acquire-recursive-lock lock :wait nil))
    (is (recursive-lock-p (release-recursive-lock lock)))))

#+#.(bt2::implemented-p* 'bt2:acquire-recursive-lock)
(test acquire-recursive-lock.try-lock
  (let ((lock (make-recursive-lock)))
    (make-thread (lambda ()
                   (acquire-recursive-lock lock)
                   (sleep 60)))
    (sleep 3)
    (is-false (acquire-recursive-lock lock :wait nil))))

#+#.(bt2::implemented-p* 'bt2:acquire-recursive-lock)
(test acquire-recursive-lock.timeout-expires
  (let ((lock (make-recursive-lock)))
    (make-thread (lambda ()
                   (acquire-recursive-lock lock)
                   (sleep 60)))
    (sleep 3)
    (is (null (acquire-recursive-lock lock :timeout .1)))))

(test with-recursive-lock-held.timeout-no-contention-acquired
  (let ((lock (make-recursive-lock)))
    (is (eql :ok (with-recursive-lock-held (lock :timeout .1) :ok)))))

#+#.(bt2::implemented-p* 'bt2:acquire-recursive-lock)
(test with-recursive-lock-held.timeout-expires
  (let ((lock (make-recursive-lock)))
    (make-thread (lambda ()
                   (acquire-recursive-lock lock)
                   (sleep 60)))
    (sleep 1)
    (is (eql :timeout
             (block ok
               (with-recursive-lock-held (lock :timeout .1)
                 (return-from ok :ok))
               :timeout)))))


;;;
;;; Semaphores
;;;

#+#.(bt2::implemented-p* 'bt2:signal-semaphore)
(progn
  (test semaphore.typed
    (is (typep (make-semaphore) 'semaphore))
    (is (semaphorep (make-semaphore)))
    (is (not (semaphorep (make-lock)))))

  (test semaphore.signal
    (let ((sem (make-semaphore)))
      (make-thread (lambda () (sleep 0.4) (signal-semaphore sem)))
      (is (eql t (wait-on-semaphore sem)))))

  (test semaphore-wait.timeout
    (let* ((sem (make-semaphore)))
      (is (null (wait-on-semaphore sem :timeout 0)))
      (is (null (wait-on-semaphore sem :timeout 0.2)))))
  
  (test semaphore.signal-n-of-m
    (let* ((sem (make-semaphore :count 1))
           (lock (make-lock))
           (count 0)
           (waiter (lambda ()
                     (wait-on-semaphore sem)
                     (with-lock-held (lock) (incf count)))))
      (make-thread (lambda () (sleep 0.2)
                     (signal-semaphore sem :count 3)))
      (dotimes (v 5) (make-thread waiter))
      (sleep 0.3)
      (is (= count 4))
      ;; release other waiters
      (is (eql t (signal-semaphore sem :count 2)))
      (sleep 0.1)
      (is (= count 5)))))


;;;
;;; Condition variables
;;;

#+#.(bt2::implemented-p* 'bt2:condition-wait)
(test condition-variable.typed
  (is (typep (make-condition-variable) 'condition-variable))
  (is (condition-variable-p (make-condition-variable)))
  (is (not (condition-variable-p (make-lock)))))

#+#.(bt2::implemented-p* 'bt2:condition-wait)
(test condition-variable.concurrency
  (setf *shared* 0)
  (let ((cv (make-condition-variable)))
    (flet ((worker (i)
             (with-lock-held (*lock*)
               (loop
                 until (= i *shared*)
                 do (sleep (random .1))
                    (condition-wait cv *lock*))
               (incf *shared*))
             (condition-broadcast cv)))
      (let ((num-procs 30))
        (dotimes (i num-procs)
          (let ((i (- num-procs i 1)))
            (make-thread (lambda ()
                           (sleep (random 1))
                           (funcall #'worker i))
                         :name (format nil "Proc #~D" i))))
        (with-lock-held (*lock*)
          (loop
            until (= num-procs *shared*)
            do (condition-wait cv *lock*)))
        (is (equal num-procs *shared*))))))

#+#.(bt2::implemented-p* 'bt2:condition-wait :timeout)
(test condition-wait.timeout
  (let ((lock (make-lock))
        (cv (make-condition-variable))
        (flag nil))
    (make-thread (lambda () (sleep 0.4) (setf flag t)))
    (with-lock-held (lock)
      (condition-wait cv lock :timeout 0.2)
      (is (null flag))
      (sleep 0.4)
      (is (eq t flag)))))


;;;
;;; Timeouts
;;;

(test with-timeout.return-value
  (is (eql :foo (with-timeout (5) :foo))))

(test with-timeout.signals
  (signals timeout (with-timeout (1) (sleep 5))))

(test with-timeout.non-interference
  (flet ((sleep-with-timeout (s)
           (with-timeout (4) (sleep s))))
    (finishes
      (progn
        (sleep-with-timeout 3)
        (sleep-with-timeout 3)))))


;;;
;;; Atomics
;;;

#+(or abcl allegro ccl ecl lispworks sbcl)
(test atomic-integer-incf-decf.return-value
  (let ((aint (make-atomic-integer :value 0)))
    (is (= (atomic-integer-incf aint 5) 5))
    (is (= (atomic-integer-decf aint 1) 4))))

#+(or abcl allegro ccl ecl lispworks sbcl)
(test atomic-integer-cas.return-value
  (let ((aint (make-atomic-integer :value 4)))
    (is (null (atomic-integer-cas aint 0 100)))
    (is (eql t (atomic-integer-cas aint 4 7)))))

#+(or abcl allegro ccl ecl lispworks sbcl)
(test atomic-integer.concurrency
  (let* ((aint (make-atomic-integer :value 1000000))
         (thread-inc
           (make-thread (lambda ()
                          (dotimes (i 1000000)
                            (atomic-integer-incf aint)))))
         (thread-dec
           (make-thread (lambda ()
                          (dotimes (i 1000000)
                            (atomic-integer-decf aint))))))
    (join-thread thread-inc)
    (join-thread thread-dec)
    (is (= (atomic-integer-value aint) 1000000))))
