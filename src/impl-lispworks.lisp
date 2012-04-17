;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

;;; documentation on the LispWorks Multiprocessing interface can be found at
;;; http://www.lispworks.com/documentation/lw445/LWUG/html/lwuser-156.htm

(deftype thread ()
  'mp:process)

;;; Thread Creation

(defun start-multiprocessing ()
  (mp:initialize-multiprocessing))

(defun %make-thread (function name)
  (mp:process-run-function name nil function))

(defun current-thread ()
  #-#.(cl:if (cl:find-symbol (cl:string '#:get-current-process) :mp) '(and) '(or))
  mp:*current-process*
  ;; introduced in LispWorks 5.1
  #+#.(cl:if (cl:find-symbol (cl:string '#:get-current-process) :mp) '(and) '(or))
  (mp:get-current-process))

(defun threadp (object)
  (or (mp:process-p object)
      ;; removed in LispWorks 6.1
      #+#.(cl:if (cl:find-symbol (cl:string '#:simple-process-p) :mp) '(and) '(or))
      (mp:simple-process-p object)))

(defun thread-name (thread)
  (mp:process-name thread))

;;; Resource contention: locks and recursive locks

(defun make-lock (&optional name)
  (mp:make-lock :name (or name "Anonymous lock")
                #-(or lispworks4 lispworks5) :recursivep
                #-(or lispworks4 lispworks5) nil))

(defun acquire-lock (lock &optional (wait-p t))
  (mp:process-lock lock nil
                   (cond ((null wait-p)         0)
                         ((numberp wait-p) wait-p)
                         (t                   nil))))

(defun release-lock (lock)
  (mp:process-unlock lock))

(defmacro with-lock-held ((place) &body body)
  `(mp:with-lock (,place) ,@body))

(defun make-recursive-lock (&optional name)
  (mp:make-lock :name (or name "Anonymous recursive lock")
                #-(or lispworks4 lispworks5) :recursivep
                #-(or lispworks4 lispworks5) t))

(defun acquire-recursive-lock (lock &optional (wait-p t))
  (acquire-lock lock wait-p))

(defun release-recursive-lock (lock)
  (release-lock lock))

(defmacro with-recursive-lock-held ((place) &body body)
  `(mp:with-lock (,place) ,@body))

;;; Resource contention: condition variables

#+(or lispworks6)
(defun make-condition-variable (&key name)
  (mp:make-condition-variable :name (or name "Anonymous condition variable")))

#+(or lispworks6)
(defun condition-wait (condition-variable lock)
  (mp:condition-variable-wait condition-variable lock))

#+(or lispworks6)
(defun condition-notify (condition-variable)
  (mp:condition-variable-signal condition-variable))

(defun thread-yield ()
  (mp:process-allow-scheduling))

;;; Introspection/debugging

(defun all-threads ()
  (mp:list-all-processes))

(defun interrupt-thread (thread function &rest args)
  (apply #'mp:process-interrupt thread function args))

(defun destroy-thread (thread)
  (signal-error-if-current-thread thread)
  (mp:process-kill thread))

(defun thread-alive-p (thread)
  (mp:process-alive-p thread))

(defun join-thread (thread)
  #-#.(cl:if (cl:find-symbol (cl:string '#:process-join) :mp) '(and) '(or))
  (mp:process-wait (format nil "Waiting for thread ~A to complete" thread)
                   (complement #'mp:process-alive-p)
                   thread)
  #+#.(cl:if (cl:find-symbol (cl:string '#:process-join) :mp) '(and) '(or))
  (mp:process-join thread))

(mark-supported)
