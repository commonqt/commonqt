(defsystem :qt-tutorial
    :serial t
    :pathname (merge-pathnames
               "tutorial/"
               (make-pathname :name nil :type nil :defaults *load-truename*))
    :components ((:file "t14")
                 (:file "conv"))
    :depends-on (:qt))
