#|
Copyright 2006,2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(defpackage bordeaux-threads-system
  (:use #:cl #:asdf))

(in-package :bordeaux-threads-system)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+allegro (require :process)
  #+corman  (require :threads))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or (and allegro multiprocessing)
        armedbear
        (and cmu mp)
        scl
        corman
        (and digitool ccl-5.1)
        (and ecl threads)
        lispworks
        (and openmcl openmcl-native-threads)
        (and sbcl sb-thread)
        (and clisp mt))
  (pushnew :thread-support *features*))

(defsystem bordeaux-threads
  :description ""
  :long-description ""
  :author "Greg Pfeil <greg@technomadic.org>"
  ;; based on original Bordeaux-MP spec by Dan Barlow <dan@telent.net>
  ;; contributors:
  ;; Attila Lendvai <attila.lendvai@gmail.com>
  ;; - better handling of unsupported Lisps
  ;; Vladimir Sekissov <svg@surnet.ru>
  ;; - fixes for CMUCL implementation
  ;; Pierre Thierry <nowhere.man@levallois.eu.org>
  ;; - added license information
  ;; Stelian Ionescu <sionescu@common-lisp.net>
  ;; - finished conversion from generic functions
  ;; - enabled running thread-safe code in unthreaded lisps
  ;; Douglas Crosher <dtc@scieneer.com>
  ;; - added Scieneer Common Lisp support
  :licence "MIT"
  :version "0.5.1"
  :components ((:module "src"
                        :serial t
                        :components
                        ((:file "bordeaux-threads")
                         (:file #+(and thread-support allegro)   "allegro"
                                #+(and thread-support armedbear) "armedbear"
                                #+(and thread-support cmu)       "cmu"
                                #+(and thread-support scl)       "scl"
                                #+(and thread-support corman)    "corman"
                                #+(and thread-support digitool)  "mcl"
                                #+(and thread-support ecl)       "ecl"
                                #+(and thread-support lispworks) "lispworks"
                                #+(and thread-support openmcl)   "openmcl"
                                #+(and thread-support sbcl)      "sbcl"
                                #+(and thread-support clisp)     "clisp"
                                #-thread-support                 "unsupported")
                         (:file "default-implementations")
                         #+(and thread-support
                                (or armedbear ecl lispworks digitool))
                         (:file "condition-variables"))))
  :in-order-to ((test-op (load-op bordeaux-threads-test)))
  :perform (test-op :after (op c)
             (describe
              (funcall (intern (string '#:run-tests) :lift)
                       :suite (intern (string '#:test-bordeaux-threads)
                                      :bordeaux-threads-test)))))

(defmethod operation-done-p ((op test-op)
                             (c (eql (find-system :bordeaux-threads))))
  (values nil))

(defsystem bordeaux-threads-test
  :depends-on (:bordeaux-threads :lift)
  :components ((:module "test" :components ((:file "bordeaux-threads-test")))))
