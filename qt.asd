(defpackage :qt-system
  (:use :cl :asdf))

(in-package :qt-system)

(defclass cpp->so (source-file)
  ())

(defmethod source-file-type ((c cpp->so) (s module)) "cpp")

(defmethod output-files ((operation compile-op) (c cpp->so))
  (list (merge-pathnames "libcommonqt.so" (component-pathname c))))

(defmethod perform ((o load-op) (c cpp->so))
  t)

(defmethod perform ((o compile-op) (c cpp->so))
  (unless (operation-done-p o c)
    (when (find-package :qt)
      (set (find-symbol "*LOADED*" :qt) nil))
    (unless (zerop (run-shell-command
                    "if which gmake; then gmake -C ~S; else make -C ~:*~S; fi"
                    (namestring
                     (make-pathname :name nil
                                    :type nil
                                    :defaults (component-pathname c)))))
      (error 'operation-error :component c :operation o))))

(defsystem :qt
    :serial t
    :components ((cpp->so "commonqt")
                 (:file "package")
                 (:file "ffi")
                 (:file "info")
                 (:file "reader")
                 (:file "call")
                 (:file "marshal")
                 (:file "unmarshal")
                 (:file "meta")
                 (:file "property")
                 (:file "test"))
    :depends-on (:cffi :named-readtables :cl-ppcre :alexandria :closer-mop
                       :iterate :trivial-garbage))
