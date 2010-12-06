(defsystem :qt-tutorial
  :serial t
  :pathname #-asdf2 (merge-pathnames
                     "tutorial/"
                     (make-pathname :name nil :type nil :defaults *load-truename*))
  #+asdf2 "tutorial/"
  :components ((:file "t14")
               (:file "conv"))
  :depends-on (:qt))
