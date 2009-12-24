#|
Copyright 2006,2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(defsystem :bordeaux-threads-test
  :depends-on (:bordeaux-threads :lift)
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :components ((:module "test"
                :components ((:file "bordeaux-threads-test")))))
