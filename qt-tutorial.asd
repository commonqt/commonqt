(defsystem :qt-tutorial
    :serial t
    :pathname (merge-pathnames
               "tutorial/"
               (make-pathname :name nil :type nil :defaults *load-truename*))
    :components ((:file "t6")
                 (:file "t7")
                 (:file "t8")
                 (:file "t9")
                 (:file "t10")
                 (:file "t11")
                 (:file "t12")
                 (:file "t13")
                 (:file "t14"))
    :depends-on (:qt))
