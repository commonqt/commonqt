;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; Copyright (c) 2009 David Lichteblau. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :qt)
#+sbcl (declaim (optimize (debug 2)))

(defvar *loaded* nil)

(defun load-smoke-library ()
  (cffi:load-foreign-library
   (namestring (merge-pathnames "libcommonqt.so"
                                (asdf::component-relative-pathname
                                 (asdf:find-system :qt)))))
  (setf *loaded* t))

;; On SBCL/win32, :LINKAGE-TABLE is on *FEATURES*, but that's a lie, it
;; can't actually process FFI definitions before the library has been
;; loaded.
#-(and sbcl (and linkage-table (not windows)))
(load-smoke-library)

(defmacro defcfun (name ret &rest args)
  `(cffi:defcfun (,name ,(intern (string-upcase name) :qt)) ,ret ,@args))

(cffi:defcvar ("qt_Smoke" qt_Smoke) :pointer)
(cffi:defcvar ("qtwebkit_Smoke" qtwebkit_Smoke) :pointer)

(defcfun "sw_init" :void)

(defcfun "sw_smoke" :void
  (smoke :pointer)
  (data :pointer)
  (deletion-callback :pointer)
  (method-callback :pointer)
  (child-callback :pointer))

(defcfun "sw_windows_version" :int)

(defcfun "sw_map_children" :void
  (obj :pointer)
  (cb :pointer))

(defcfun "sw_make_qpointer" :pointer
  (target :pointer))

(defcfun "sw_qpointer_is_null" :char
  (qp :pointer))

(defcfun "sw_delete_qpointer" :void
  (qp :pointer))

(defcfun "sw_make_qstring" :pointer
  (str :string))

(defcfun "sw_delete_qstring" :void
  (qstring :pointer))

(defcfun "sw_make_metaobject" :pointer
  (parent :pointer)
  (str :pointer)
  (data :pointer))

(defcfun "sw_delete" :void
  (stack :pointer))

(defcfun "sw_qstring_to_utf8" :pointer
  (obj :pointer))

(cffi:defcstruct |struct SmokeData|
  (classes :pointer)
  (nclasses :short)
  (methods :pointer)
  (nmethods :short)
  (methodmaps :pointer)
  (nmethodmaps :short)
  (methodnames :pointer)
  (nmethodnames :short)
  (types :pointer)
  (ntypes :short)
  (inheritanceList :pointer)
  (argumentList :pointer)
  (ambiguousMethodList :pointer)
  (castFn :pointer)
  (thin :pointer)
  (fat :pointer))

(cffi:defcunion |union StackItem|
  (ptr :pointer)
  (bool :char)
  (char :char)
  (uchar :unsigned-char)
  (short :short)
  (ushort :unsigned-short)
  (int :int)
  (uint :uint)
  (long :long)
  (ulong :ulong)
  (float :float)
  (double :double)
  (enum :int)
  (class :pointer))

(cffi:defcstruct |struct Method|
  (classid :short)
  (name :short)
  (args :short)
  (numargs :unsigned-char)
  (flags :unsigned-char)
  (ret :short)
  (methodForClassFun :short))

(cffi:defcstruct |struct MethodMap|
  (classid :short)
  (name :short)
  (methodid :short))

(cffi:defcstruct |struct Class|
  (className :string)
  (external :char)
  (parents :short)
  (classfn :pointer) ;; void (*classfn)(Index method, void *obj, Stack args)
  (enumfn :pointer)
  (flags :short))

(cffi:defcstruct |struct Type|
  (name :string)
  (classid :short)
  (flags :short))

(defvar *callbacks* (make-hash-table :test 'equal))

#+(and ccl windows)
(in-package :cffi-sys)
#+(and ccl windows)
(format t "patching cffi for stdcall callbacks support~%")
#+(and ccl windows)
(defmacro %defcallback (name rettype arg-names arg-types body
                        &key calling-convention)
  (let ((cb-name (intern-callback name)))
    `(progn
       (defcallback ,cb-name
           (,@ (ecase calling-convention
                 ((:cdecl nil) '())
                 ((:stdcall) '(:discard-stack-args)))
               ,@(mapcan (lambda (sym type)
                           (list (convert-foreign-type type) sym))
                         arg-names arg-types)
               ,(convert-foreign-type rettype))
         ,body)
       (setf (gethash ',name *callbacks*) (symbol-value ',cb-name)))))
#+(and ccl windows) (in-package :qt)

(defmacro defcallback (name ret (&rest args) &body body)
  `(cffi:defcallback (,name #+(and ccl windows) :calling-convention
                            #+(and ccl windows) :stdcall)
       ,ret ,args ,@body))

(defcallback deletion-callback
    :void
    ((smoke :pointer)
     (obj :pointer))
  ;; Just dispatch to an ordinary function for debugging purposes.
  ;; Redefinition of a callback wouldn't affect the existing C++ code,
  ;; redefinition of the function does.
  (%deletion-callback obj))

(defcallback method-invocation-callback
    :int
    ((smoke :pointer)
     (method :short)
     (obj :pointer)
     (args :pointer)
     (abstractp :int))
  ;; Just dispatch to an ordinary function for debugging purposes.
  ;; Redefinition of a callback wouldn't affect the existing C++ code,
  ;; redefinition of the function does.
  (%method-invocation-callback smoke method obj args abstractp))

(defcallback child-callback
    :void
    ((smoke :pointer)
     (added :char)                      ;bool
     (obj :pointer))
  ;; Just dispatch to an ordinary function for debugging purposes.
  ;; Redefinition of a callback wouldn't affect the existing C++ code,
  ;; redefinition of the function does.
  (%child-callback added obj))

(defvar *ptr-callback*)

(defcallback ptr-callback
    :void
    ((obj :pointer))
  (funcall *ptr-callback* obj))
