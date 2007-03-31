#|
Copyright 2006,2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :threads))

;;; Thread Creation

(defmethod make-thread (function &key name)
  (declare (ignore name))
  (threads:create-thread function))

(defmethod current-thread ()
  threads:*current-thread*)

;;; Introspection/debugging

(defmethod destroy-thread (thread)
  (threads:terminate-thread thread))

(mark-supported)
