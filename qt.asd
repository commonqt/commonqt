(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :trivial-features))

(defsystem :qt
  :description "Interface for the Qt GUI framework"
  :license "BSD"
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "ffi")
   (:file "reader")
   (:file "meta-classes")
   (:file "classes")
   (:file "info")
   (:file "marshal")
   (:file "unmarshal")
   (:file "primitive-call")
   (:file "call")
   (:file "meta")
   (:file "qvariant")
   (:file "property")
   (:file "qlist")
   (:file "qapp")
   (:file "connect"))
  :defsystem-depends-on (:trivial-features)
  :depends-on (:cffi :named-readtables :cl-ppcre :alexandria
               :closer-mop :qt-libs
               :iterate :trivial-garbage #+darwin :bordeaux-threads))
