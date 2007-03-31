#|
Copyright 2006,2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package :bordeaux-threads)

(defmethod current-thread ()
  nil)

(cerror "Ignore and continue"
        "There is no Bordeaux-Threads support for your implementation, some features may not work.
Feel free to implement it, or bug one of the maintainers to do so if your lisp supports threads at all.")