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

#-(and sbcl linkage-table)
(load-smoke-library)

(defmacro defcfun (name ret &rest args)
  `(cffi:defcfun (,name ,(intern (string-upcase name) :qt)) ,ret ,@args))

(defcfun "sw_init" :pointer
  (data :pointer)
  (deletion-callback :pointer)
  (method-callback :pointer)
  (child-callback :pointer))

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
  (castFn :pointer))

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
  (parents :short)
  (classfn :pointer) ;; void (*classfn)(Index method, void *obj, Stack args)
  (enumfn :pointer)
  (flags :short))

(cffi:defcstruct |struct Type|
  (name :string)
  (classid :short)
  (flags :short))

(cffi:defcallback deletion-callback
    :void
    ((obj :pointer))
  ;; Just dispatch to an ordinary function for debugging purposes.
  ;; Redefinition of a callback wouldn't affect the existing C++ code,
  ;; redefinition of the function does.
  (%deletion-callback obj))

(cffi:defcallback method-invocation-callback
    :int
    ((method :short)
     (obj :pointer)
     (args :pointer)
     (abstractp :int))
  ;; Just dispatch to an ordinary function for debugging purposes.
  ;; Redefinition of a callback wouldn't affect the existing C++ code,
  ;; redefinition of the function does.
  (%method-invocation-callback method obj args abstractp))

(cffi:defcallback child-callback
    :void
    ((added :char)                      ;bool
     (obj :pointer))
  ;; Just dispatch to an ordinary function for debugging purposes.
  ;; Redefinition of a callback wouldn't affect the existing C++ code,
  ;; redefinition of the function does.
  (%child-callback added obj))
