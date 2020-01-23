(defsystem :qt+libs
  :description "Interface for the Qt GUI framework (precompiled libraries)"
  :license "BSD"
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "qt-libs-utils")
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
   (:file "connect")
   (:file "qt-libs"))
  :defsystem-depends-on (:trivial-features)
  :depends-on (:cffi :named-readtables :cl-ppcre :alexandria
               :closer-mop :qt-libs
               :iterate :trivial-garbage
               (:feature :darwin :bordeaux-threads)))
