(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :trivial-features))

;;; .cpp

(defclass cpp->so (source-file)
  ())

(defmethod source-file-type ((c cpp->so) (s module)) "cpp")

(defmethod output-files ((operation compile-op) (c cpp->so))
  (values
   (list (make-pathname :name "libcommonqt"
                        :type #-darwin "so" #+darwin "dylib"
                        :defaults (component-pathname c)))
   ;; libcommonqt.so* files are never moved to separate FASL directory
   t))

(defmethod perform ((o load-op) (c cpp->so))
  t)

(defmethod perform ((o compile-op) (c cpp->so))
  (when (find-package :qt)
    (set (find-symbol (symbol-name '*loaded*) :qt) nil))
  (unless (zerop (run-shell-command
                  "if which gmake; then gmake -C ~S; else make -C ~:*~S; fi ~
                   && touch ~s"
                  (namestring
                   (make-pathname :name nil
                                  :type nil
                                  :defaults (component-pathname c)))
                  (namestring (car (output-files o c)))))
    (error 'operation-error :component c :operation o)))

;;; qmake

(defclass makefile (source-file)
  ())

(defmethod source-file-type ((c makefile) (s module)) nil)

(defmethod output-files ((operation compile-op) (c makefile))
  (values (list (make-pathname :name "Makefile"
                               :type nil
                               :defaults (component-pathname c)))
          t))

(defmethod perform ((o load-op) (c makefile))
  t)

(defmethod perform ((o compile-op) (c makefile))
  (when (find-package :qt)
    (set (find-symbol (symbol-name '*loaded*) :qt) nil))
  (unless (zerop (run-shell-command
                  "command -v qmake-qt4 || command -v qmake"))
    (error "No qmake found."))
  (unless (zerop (run-shell-command
                  "`command -v qmake-qt4 || command -v qmake` ~A~S -o ~S"
                  #+darwin "-spec macx-g++ " #-darwin ""
                  (namestring (component-pathname c))
                  (namestring (output-file o c))))
    (error 'operation-error :component c :operation o)))

;;; system

(defsystem :qt
  :description "Interface for the Qt GUI framework"
  :license "BSD"
  :components
  (#-windows
   (:module "so"
    :pathname ""
    :serial t
    :components
    ((makefile "commonqt.pro")
     (:static-file "commonqt.h")
     (cpp->so "commonqt" :depends-on ("commonqt.h"))))
   (:module "lisp"
    :pathname ""
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
     (:file "connect"))))
  :defsystem-depends-on (:trivial-features)
  :depends-on (:cffi :named-readtables :cl-ppcre :alexandria
               :closer-mop
	       :iterate :trivial-garbage
	       #+(or darwin (not (or sbcl ccl))) :bordeaux-threads))
