(defpackage :qt-test-system
  (:use :cl :asdf))

(in-package :qt-test-system)
;;; system

(defsystem :qt-test
    :serial t
    :pathname #-asdf2 (merge-pathnames #p"test/" *load-truename*)
              #+asdf2 "test/"
    :components ((:file "package")
                 (:file "tests")
                 (:file "microbench"))
    :depends-on (:qt :alexandria :iterate :trivial-garbage :rt :bordeaux-threads))
