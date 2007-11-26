#|
Copyright 2006,2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(defpackage bordeaux-threads-system
  (:use #:cl #:asdf))

(in-package :bordeaux-threads-system)

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
  :licence "MIT"
  :version "0.2.2"
  :serial t
  :components ((:module "src"
                        :components ((:file "bordeaux-threads")
                                     (:file #+allegro "allegro"
                                            #+armedbear "armedbear"
                                            #+cmu "cmu"
                                            #+corman "corman"
                                            #+(and digitool ccl-5.1) "mcl"
                                            #+ecl "ecl"
                                            #+lispworks "lispworks"
                                            #+openmcl "openmcl"
                                            #+(and sbcl sb-thread) "sbcl"
                                            #-(or allegro armedbear cmu corman
                                                  (and digitool ccl-5.1) ecl
                                                  lispworks openmcl
                                                  (and sbcl sb-thread))
                                            "unsupported")
                                     (:file "default-implementations")
                                     #+(or armedbear ecl lispworks)
                                     (:file "condition-variables"))))
  :in-order-to ((test-op (load-op bordeaux-threads-test)))
  :perform (test-op :after (op c)
                    (describe
                     (funcall
                      (intern (symbol-name (read-from-string "run-tests"))
                              :lift)
                      :suite (intern
                              (symbol-name
                               (read-from-string "test-bordeaux-threads"))
                              :bordeaux-threads-test)))))

(defmethod operation-done-p ((op test-op)
                             (c (eql (find-system :bordeaux-threads))))
  (values nil))

(defsystem bordeaux-threads-test
  :depends-on (bordeaux-threads lift)
  :components ((:module "test" :components ((:file "bordeaux-threads-test")))))
