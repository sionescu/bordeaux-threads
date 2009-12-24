#|
Copyright 2006,2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(defsystem :bordeaux-threads-test
  :depends-on (:bordeaux-threads :lift)
  :components ((:module "test"
                :components ((:file "bordeaux-threads-test")))))
