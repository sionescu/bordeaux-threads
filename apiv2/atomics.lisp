;;;; -*- indent-tabs-mode: nil -*-

(in-package :bordeaux-threads-2)

(defmacro atomic-cas (place old new)
  (declare (ignorable place old new))
  #+allegro `(excl:atomic-conditional-setf ,place ,new ,old)
  #+ccl `(ccl::conditional-store ,place ,old ,new)
  #+ecl (with-gensyms (tmp)
          `(let ((,tmp ,old))
             (eql ,tmp (mp:compare-and-swap ,place ,tmp ,new))))
  #+lispworks `(system:compare-and-swap ,place ,old ,new)
  #+sbcl (with-gensyms (tmp)
           `(let ((,tmp ,old))
              (eql ,tmp (sb-ext:compare-and-swap ,place ,old ,new))))
  #-(or allegro ccl ecl lispworks sbcl)
  (signal-not-implemented 'atomic-cas))

(defmacro atomic-decf (place &optional (delta 1))
  (declare (ignorable place delta))
  #+allegro `(excl:decf-atomic ,place ,delta)
  #+ccl `(ccl::atomic-incf-decf ,place (- ,delta))
  #+ecl `(- (mp:atomic-decf ,place ,delta) ,delta)
  #+lispworks `(system:atomic-decf ,place ,delta)
  #+sbcl `(- (sb-ext:atomic-decf ,place ,delta) ,delta)
  #-(or allegro ccl ecl lispworks sbcl)
  (signal-not-implemented 'atomic-decf))

(defmacro atomic-incf (place &optional (delta 1))
  (declare (ignorable place delta))
  #+allegro `(excl:incf-atomic ,place ,delta)
  #+ccl `(ccl::atomic-incf-decf ,place ,delta)
  #+ecl `(+ (mp:atomic-incf ,place ,delta) ,delta)
  #+lispworks `(system:atomic-incf ,place ,delta)
  #+sbcl `(+ (sb-ext:atomic-incf ,place ,delta) ,delta)
  #-(or allegro ccl ecl lispworks sbcl)
  (signal-not-implemented 'atomic-incf))

(deftype %atomic-integer-value ()
  '(unsigned-byte 64))

(defstruct (atomic-integer
            (:constructor %make-atomic-integer ()))
  "Wrapper for an (UNSIGNED-BYTE 64) that allows atomic
increment, decrement and swap."
  #+(or allegro ccl ecl lispworks)
  (cell (make-array 1 :element-type t))
  #+(or clisp sbcl)
  (cell 0 :type %atomic-integer-value)
  #+clisp
  (%lock (%make-lock nil) :type native-lock))

(defmethod print-object ((aint atomic-integer) stream)
  (print-unreadable-object (aint stream :type t :identity t)
    (format stream "~S" (atomic-integer-value aint))))

#-(or allegro ccl clisp ecl lispworks sbcl)
(mark-not-implemented 'make-atomic-integer)
(defun make-atomic-integer (&key (value 0))
  (check-type value %atomic-integer-value)
  #+(or allegro ccl clisp ecl lispworks sbcl)
  (let ((aint (%make-atomic-integer)))
    (setf (atomic-integer-value aint) value)
    aint)
  #-(or allegro ccl clisp ecl lispworks sbcl)
  (signal-not-implemented 'make-atomic-integer))

(defun atomic-integer-cas (atomic-integer old new)
  (declare (type atomic-integer atomic-integer)
           (type %atomic-integer-value old new)
           (optimize (safety 0) (speed 3)))
  #-clisp
  (atomic-cas #-sbcl (svref (atomic-integer-cell atomic-integer) 0)
              #+sbcl (atomic-integer-cell atomic-integer)
              old new)
  #+clisp
  (%with-lock ((atomic-integer-%lock atomic-integer) nil)
    (cond
      ((= old (slot-value atomic-integer 'cell))
       (setf (slot-value atomic-integer 'cell) new)
       t)
      (t nil))))

(defun atomic-integer-decf (atomic-integer &optional (delta 1))
  (declare (type atomic-integer atomic-integer)
           (type %atomic-integer-value delta)
           (optimize (safety 0) (speed 3)))
  #-clisp
  (atomic-decf #-sbcl (svref (atomic-integer-cell atomic-integer) 0)
               #+sbcl (atomic-integer-cell atomic-integer)
               delta)
  #+clisp
  (%with-lock ((atomic-integer-%lock atomic-integer) nil)
    (decf (atomic-integer-cell atomic-integer) delta)))

(defun atomic-integer-incf (atomic-integer &optional (delta 1))
  (declare (type atomic-integer atomic-integer)
           (type %atomic-integer-value delta)
           (optimize (safety 0) (speed 3)))
  #-clisp
  (atomic-incf #-sbcl (svref (atomic-integer-cell atomic-integer) 0)
               #+sbcl (atomic-integer-cell atomic-integer)
               delta)
  #+clisp
  (%with-lock ((atomic-integer-%lock atomic-integer) nil)
    (incf (atomic-integer-cell atomic-integer) delta)))

(defun atomic-integer-value (atomic-integer)
  (declare (type atomic-integer atomic-integer)
           (optimize (safety 0) (speed 3)))
  #-clisp
  (progn
    #-sbcl (svref (atomic-integer-cell atomic-integer) 0)
    #+sbcl (atomic-integer-cell atomic-integer))
  #+clisp
  (%with-lock ((atomic-integer-%lock atomic-integer) nil)
    (atomic-integer-cell atomic-integer)))

(defun (setf atomic-integer-value) (newval atomic-integer)
  (declare (type atomic-integer atomic-integer)
           (type %atomic-integer-value newval)
           (optimize (safety 0) (speed 3)))
  #-clisp
  (setf #-sbcl (svref (atomic-integer-cell atomic-integer) 0)
        #+sbcl (atomic-integer-cell atomic-integer)
        newval)
  #+clisp
  (%with-lock ((atomic-integer-%lock atomic-integer) nil)
    (setf (atomic-integer-cell atomic-integer) newval)))

(defstruct queue
  (vector (make-array 7 :adjustable t :fill-pointer 0) :type vector)
  (lock (%make-lock nil) :type native-lock))

(defun queue-drain (queue)
  (%with-lock ((queue-lock queue) nil)
    (shiftf (queue-vector queue)
            (make-array 7 :adjustable t :fill-pointer 0))))

(defun queue-dequeue (queue)
  (%with-lock ((queue-lock queue) nil)
    (let ((vector (queue-vector queue)))
      (if (zerop (length vector))
          nil
          (vector-pop vector)))))

(defun queue-enqueue (queue value)
  (%with-lock ((queue-lock queue) nil)
    (vector-push-extend value (queue-vector queue))))
