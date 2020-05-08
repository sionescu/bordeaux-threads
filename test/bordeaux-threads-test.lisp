#|
Copyright 2006,2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(defpackage bordeaux-threads/test
  (:use #:cl #:bordeaux-threads #:fiveam)
  (:shadow #:with-timeout))

(in-package #:bordeaux-threads/test)

(def-suite :bordeaux-threads)
(def-fixture using-lock () 
  (let ((lock (make-lock)))
    (&body)))
(in-suite :bordeaux-threads)

(test should-have-current-thread
  (is (current-thread)))

(test current-thread-identity
  (let* ((box (list nil))
         (thread (make-thread (lambda ()
                                (setf (car box) (current-thread))))))
    (join-thread thread)
    (is (eql (car box) thread))))

(test join-thread-return-value
  (is (eql 0 (join-thread (make-thread (lambda () 0))))))

(test should-identify-threads-correctly
  (is (threadp (current-thread)))
  (is (threadp (make-thread (lambda () t) :name "foo")))
  (is (not (threadp (make-lock)))))

(test should-retrieve-thread-name
  (is (equal "foo" (thread-name (make-thread (lambda () t) :name "foo")))))

(test interrupt-thread
  (let* ((box (list nil))
         (thread (make-thread (lambda ()
                                (setf (car box)
                                      (catch 'new-thread
                                        (sleep 60)
                                        'not-interrupted))))))
    (sleep 1)
    (interrupt-thread thread (lambda ()
                               (throw 'new-thread 'interrupted)))
    (join-thread thread)
    (is (eql 'interrupted (car box)))))

(test should-lock-without-contention
  (with-fixture using-lock ()
    (is (acquire-lock lock t))
    (release-lock lock)
    (is (acquire-lock lock nil))
    (release-lock lock)))

#-(or allegro sbcl)
(def-test acquire-recursive-lock ()
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

(defun set-equal (set-a set-b)
  (and (null (set-difference set-a set-b))
       (null (set-difference set-b set-a))))

(test default-special-bindings
  (locally (declare (special *a* *c*))
    (let* ((the-as 50) (the-bs 150) (*b* 42)
           some-a some-b some-other-a some-other-b
           (*default-special-bindings*
            `((*a* . (funcall ,(lambda () (incf the-as))))
              (*b* . (funcall ,(lambda () (incf the-bs))))
              ,@*default-special-bindings*))
           (threads (list (make-thread
                           (lambda ()
                             (setf some-a *a* some-b *b*)))
                          (make-thread
                           (lambda ()
                             (setf some-other-a *a*
                                   some-other-b *b*))))))
      (declare (special *b*))
      (thread-yield)
      (is (not (boundp '*a*)))
      (loop while (some #'thread-alive-p threads)
            do (thread-yield))
      (is (set-equal (list some-a some-other-a) '(51 52)))
      (is (set-equal (list some-b some-other-b) '(151 152)))
      (is (not (boundp '*a*))))))


(defparameter *shared* 0)
(defparameter *lock* (make-lock))

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


(defparameter *condition-variable* (make-condition-variable))

(test condition-variable
  (setf *shared* 0)
  (flet ((worker (i)
           (with-lock-held (*lock*)
             (loop
               until (= i *shared*)
               do (condition-wait *condition-variable* *lock*))
             (incf *shared*))
           (condition-notify *condition-variable*)))
    (let ((num-procs 100))
      (dotimes (i num-procs)
        ;; create a new binding to protect against implementations that
        ;; mutate instead of binding the loop variable
        (let ((i i))
          (make-thread (lambda ()
                         (funcall #'worker i))
                       :name (format nil "Proc #~D" i))))
      (with-lock-held (*lock*)
        (loop
          until (= num-procs *shared*)
          do (condition-wait *condition-variable* *lock*)))
      (is (equal num-procs *shared*)))))

;; Generally safe sanity check for the locks and single-notify
#+(and lispworks (or lispworks4 lispworks5))
(test condition-variable-lw
  (let ((condition-variable (make-condition-variable :name "Test"))
        (test-lock (make-lock))
        (completed nil))
    (dotimes (id 6)
      (let ((id id))
        (make-thread (lambda ()
                       (with-lock-held (test-lock)
                         (condition-wait condition-variable test-lock)
                         (push id completed)
                         (condition-notify condition-variable))))))
    (sleep 2)
    (if completed
        (print "Failed: Premature passage through condition-wait")
        (print "Successfully waited on condition"))
    (condition-notify condition-variable)
    (sleep 2)
    (if (and completed
             (eql (length completed) 6)
             (equal (sort completed #'<)
                    (loop for id from 0 to 5 collect id)))
        (print "Success: All elements notified")
        (print (format nil "Failed: Of 6 expected elements, only ~A proceeded" completed)))
    (bt::with-cv-access condition-variable
      (if (and
           (not (or (car wait-tlist) (cdr wait-tlist)))
           (zerop (hash-table-count wait-hash))
           (zerop (hash-table-count unconsumed-notifications)))
          (print "Success: condition variable restored to initial state")
          (print "Error: condition variable retains residue from completed waiters")))
    (setq completed nil)
    (dotimes (id 6)
          (let ((id id))
            (make-thread (lambda ()
                           (with-lock-held (test-lock)
                             (condition-wait condition-variable test-lock)
                             (push id completed))))))
    (sleep 2)
    (condition-notify condition-variable)
    (sleep 2)
    (if (= (length completed) 1)
        (print "Success: Notify-single only notified a single waiter to restart")
        (format t "Failure: Notify-single restarted ~A items" (length completed)))
    (condition-notify condition-variable)
    (sleep 2)
    (if (= (length completed) 2)
        (print "Success: second Notify-single only notified a single waiter to restart")
        (format t "Failure: Two Notify-singles restarted ~A items" (length completed)))
    (loop for i from 0 to 5 do (condition-notify condition-variable))
    (print "Note:  In the case of any failures, assume there are outstanding waiting threads")
    (values)))

#+(or abcl allegro clisp clozure ecl lispworks6 sbcl scl)
(test condition-wait-timeout
  (let ((lock (make-lock))
        (cvar (make-condition-variable))
        (flag nil))
    (make-thread (lambda () (sleep 0.4) (setf flag t)))
    (with-lock-held (lock)
      (condition-wait cvar lock :timeout 0.2)
      (is (null flag))
      (sleep 0.4)
      (is (eq t flag)))))

(test semaphore-signal
  (let ((sem (make-semaphore)))
    (make-thread (lambda () (sleep 0.4) (signal-semaphore sem)))
    (is (not (null (wait-on-semaphore sem))))))

(test semaphore-signal-n-of-m
  (let* ((sem (make-semaphore :count 1))
         (lock (make-lock))
         (count 0)
         (waiter (lambda ()
                   (wait-on-semaphore sem)
                   (with-lock-held (lock) (incf count)))))
    (make-thread (lambda () (sleep 0.2) (signal-semaphore sem :count 3)))
    (dotimes (v 5) (make-thread waiter))
    (sleep 0.3)
    (is (= count 4))
    ;; release other waiters
    (signal-semaphore sem :count 10)
    (sleep 0.1)
    (is (= count 5))))

(test semaphore-wait-timeout
  (let ((sem (make-semaphore))
        (flag nil))
    (make-thread (lambda () (sleep 0.4) (setf flag t)))
    (is (null (wait-on-semaphore sem :timeout 0.2)))
    (is (null flag))
    (sleep 0.4)
    (is (eq t flag))))

(test semaphore-typed
  (is (typep (bt:make-semaphore) 'bt:semaphore))
  (is (bt:semaphore-p (bt:make-semaphore)))
  (is (null (bt:semaphore-p (bt:make-lock)))))

(test mailbox-send-receive
  (let ((mailbox (bt:make-mailbox :name "my smilebox")))
    (is (bt:mailbox-p mailbox))
    (is (not (bt:mailbox-p 42)))
    (is (equalp (bt:mailbox-name mailbox) "my smilebox"))
    ;; Add some
    (bt:send-message mailbox "daniel")
    (bt:send-message mailbox "ma")
    (bt:send-message mailbox 4)
    ;; Ensure FIFO order
    (is (string= "daniel" (bt:receive-message mailbox)))
    ;; Add some more
    (bt:send-message mailbox "zielone")
    (bt:send-message mailbox "koty")
    ;; Ensure counting
    (is (=  4 (bt:mailbox-count mailbox)))
    (is (null (bt:mailbox-empty-p mailbox)))
    ;; Ensure FIFO order and and other receive operations
    (is (equalp "ma" (bt:receive-message mailbox)))
    (is (equalp  4 (bt:receive-message mailbox)))
    (is (equalp  "zielone" (bt:receive-message-no-hang mailbox)))
    (is (= 1 (bt:mailbox-count mailbox)))
    (is (equalp "koty" (bt:receive-message-no-hang mailbox)))
    ;; Ensure empty box behavior
    (is (= 0 (bt:mailbox-count mailbox)))
    (is (bt:mailbox-empty-p mailbox))
    (is (null (receive-message-no-hang mailbox)))
    (is (null (receive-message mailbox :timeout 0.1)))
    (is (= 0 (bt:mailbox-count mailbox)))
    (is (bt:mailbox-empty-p mailbox))
    ;; add some new data and verify group operations
    (dotimes (v 10) (send-message mailbox v))
    (is (= 10 (bt:mailbox-count mailbox)))
    (is (not (bt:mailbox-empty-p mailbox)))
    (is (equalp '(0 1 2 3 4 5 6 7 8 9) (list-mailbox-messages mailbox)))
    (is (null (receive-pending-messages mailbox 0)))
    (signals type-error (receive-pending-messages mailbox -1))
    (is (= 10 (bt:mailbox-count mailbox)))
    (is (not (bt:mailbox-empty-p mailbox)))
    (is (equalp '(0 1 2 3 4) (receive-pending-messages mailbox 5)))
    (is (equalp '(5 6 7 8 9) (list-mailbox-messages mailbox)))
    (is (= (bt:mailbox-count mailbox) 5))
    (is (not (bt:mailbox-empty-p mailbox)))
    (is (equalp '(5 6 7 8 9) (receive-pending-messages mailbox 20)))
    (is (= (bt:mailbox-count mailbox) 0))
    (is (bt:mailbox-empty-p mailbox))
    (is (null (receive-pending-messages mailbox 20)))
    (is (= 0 (bt:mailbox-count mailbox)))
    (is (bt:mailbox-empty-p mailbox))
    ;; receive-pending-messages without optional argument
    (dotimes (v 10) (send-message mailbox v))
    (is (= 10 (bt:mailbox-count mailbox)))
    (is (not (bt:mailbox-empty-p mailbox)))
    (is (equalp '(0 1 2 3 4 5 6 7 8 9) (receive-pending-messages mailbox)))
    (is (= 0 (bt:mailbox-count mailbox)))
    (is (bt:mailbox-empty-p mailbox))))

(test mailbox-producer-consumers
  (let (;; we start from 0 and always send messages
        ;; monotonically increasing. That means that each
        ;; consumer has monotonically increasing
        ;; elements. Note that results-mailbox may have all
        ;; that jumbled up, because different consumers may
        ;; send at different times.
        (bt:mailbox (make-mailbox :initial-contents '(0)))
        ;; we will submit all results to the second mailbox,
        ;; then sort them and verify if nothing got lost. This
        ;; also gives us concurrent send-message.
        (results-mailbox (make-mailbox))
        ;; all errors in a thread should be send here. For
        ;; intance if messages are not monotonous from the
        ;; consumer perspective it is an ordering issue.
        (errors-mailbox (make-mailbox))
        ;; flag to tell consumers that we are full. We always
        ;; have a timeout in make-1-consumer (and
        ;; make-n-consumer returns immedietely), so we may
        ;; safely assume that join-thread will return.
        (konsument-je-żeby-jeść-p nil)
        ;; last added element to the mailbox called from
        ;; producer thrad and *not* thread-safe - that means
        ;; that only one producer at a time should run. For
        ;; multiple producer we resend to results-mailbox from
        ;; consumer threads.
        (last-element-added 0))
    (flet ((make-producer (n)
             (lambda ()
               (sleep 1)
               (loop for i from 1 upto n
                     do (progn (send-message mailbox (+ last-element-added i))))
               (incf last-element-added n)))
           ;; Consumers return T if they didn't put hand on any message.
           (make-1-consumer (&optional timeout sleep)
             (lambda ()
               (loop until (and konsument-je-żeby-jeść-p
                                (bt:mailbox-empty-p mailbox))
                     with last-message = nil
                     do (alexandria:when-let
                            ((message (if timeout
                                          (receive-message mailbox :timeout timeout)
                                          (receive-message-no-hang mailbox))))
                          (when (and last-message (> last-message message))
                            (send-message errors-mailbox (cons last-message message)))
                          (setf last-message message)
                          (send-message results-mailbox message)
                          (when sleep (sleep sleep)))
                     finally (return (not last-message)))))
           (make-n-consumer (batch-size &optional sleep)
             (lambda ()
               (loop until (and konsument-je-żeby-jeść-p
                                (bt:mailbox-empty-p mailbox))
                     with send = (alexandria:curry #'send-message results-mailbox)
                     with starved = t
                     do (progn
                          (alexandria:when-let
                              ((messages (receive-pending-messages mailbox batch-size)))
                            (mapc send messages)
                            (setf starved nil))
                          (when sleep (sleep sleep)))
                     finally (return starved)))))
      (let ((producer (bt:make-thread (make-producer 5000)))
            (threads (append (loop repeat 4 collect (bt:make-thread (make-1-consumer)))
                             (loop repeat 4 collect (bt:make-thread (make-1-consumer nil 0.1)))
                             (loop repeat 4 collect (bt:make-thread (make-1-consumer 1)))
                             (loop repeat 4 collect (bt:make-thread (make-n-consumer 3))))))
        (bt:join-thread producer)
        (setf konsument-je-żeby-jeść-p t)
        (mapc #'bt:join-thread threads)
        (is (bt:mailbox-empty-p errors-mailbox)
            "Some messages were not in FIFO order")
        (is (= (bt:mailbox-count results-mailbox) (1+ last-element-added))
            "Some messages were not processed")))))

(test with-timeout-return-value
  (is (eql :foo (bt:with-timeout (5) :foo))))

(test with-timeout-signals
  (signals timeout (bt:with-timeout (1) (sleep 5))))

(test with-timeout-non-interference
  (flet ((sleep-with-timeout (s)
           (bt:with-timeout (4) (sleep s))))
    (finishes
      (progn
        (sleep-with-timeout 3)
        (sleep-with-timeout 3)))))
