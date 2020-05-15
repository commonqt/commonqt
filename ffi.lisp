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

(defvar *loaded* nil)
(defvar *library-loaded-p* nil)

(defun load-libcommonqt ()
  (unless *library-loaded-p*
    (load-library "commonqt")
    (setf *library-loaded-p* t)))

#-(or ecl ccl sbcl allegro)
(load-libcommonqt)

(defmacro defcfun (name ret &rest args)
  `(cffi:defcfun (,name ,(intern (string-upcase name) :qt)) ,ret ,@args))

(defcfun "sw_smoke" :void
  (smoke :pointer)
  (data :pointer)
  (deletion-callback :pointer)
  (method-callback :pointer))

(defcfun "sw_windows_version" :int)

(defcfun "sw_make_qvector_uint" :pointer
  (uint-data :pointer)
  (size :int))

(defcfun "sw_delete_qvector_uint" :void
  (qvector-uint :pointer))

(defcfun "sw_make_qbytearray" :pointer
  (str (:string :encoding :utf-8)))

(defcfun "sw_delete_qbytearray" :void
  (str :pointer))

(defcfun "sw_make_qstring" :pointer
  (str (:string :encoding :utf-8)))

(defcfun "sw_delete_qstring" :void
  (qstring :pointer))

(defcfun "sw_qstringlist_new" :pointer)

(defcfun "sw_qstringlist_delete" :void
  (qstringlist :pointer))

(defcfun "sw_qstringlist_append" :void
  (qstringlist :pointer)
  (str (:string :encoding :utf-8)))

(defcfun "sw_qstringlist_size" :int
  (qstringlist :pointer))

(defcfun "sw_qstringlist_at" :pointer
  (qstringlist :pointer)
  (index :int))

(defcfun "sw_make_metaobject" :pointer
  (parent :pointer)
  (str :pointer)
  (data :pointer))

(defcfun "sw_delete" :void
  (stack :pointer))

(defcfun "sw_qstring_to_utf8" :pointer
  (obj :pointer))

(defcfun "sw_qstring_to_utf16" :pointer
  (obj :pointer))

(declaim (inline convert-qstring-data))
(defun convert-qstring-data (data)
  (declare (optimize speed))
  #+nil
  ;; fixme: babel doesn't get endianness right in utf-16.
  (cffi:foreign-string-to-lisp (sw_qstring_to_utf16 raw-ptr)
                               :encoding :utf-16)
  ;; handcode instead:
  (let* ((nbytes (cffi::foreign-string-length data :encoding :utf-16))
         (res (make-string (truncate nbytes 2))))
    (declare (fixnum nbytes))
    (loop for i by 2 below nbytes
          for j fixnum from 0
          for code = (cffi:mem-ref data :unsigned-short i)
          until (zerop code)
          do (setf (char res j) (code-char code)))
    res))

(defun qstring-pointer-to-lisp (raw-ptr)
  (declare (optimize speed))
  (convert-qstring-data (sw_qstring_to_utf16 raw-ptr)))

(defcfun "sw_find_name" :short
  (smoke :pointer)
  (str (:string :encoding :ascii)))

(defcfun "sw_find_class" :void
  (name (:string :encoding :ascii))
  (smoke** :pointer)
  (index** :pointer))

(defcfun "sw_id_method" :short
  (smoke :pointer)
  (class :short)
  (name :short))

(defcfun "sw_id_type" :short
  (smoke :pointer)
  (name (:string :encoding :ascii)))

(defcfun "sw_id_class" :short
  (smoke :pointer)
  (name (:string :encoding :ascii))
  (external :char))                     ;bool

(defcfun "sw_id_instance_class" :short
  (instance :pointer)
  (smoke** :pointer)
  (index** :pointer))

(defcfun "sw_override" :void
  (binding :pointer)
  (method :short)
  (override :short))

(defcfun "sw_make_dynamic_binding" :pointer
  (smoke :pointer)
  (meta-object :pointer)
  (meta-object-index :short)
  (metacall-index :short)
  (deletion-callback :pointer)
  (method-callback :pointer)
  (metacall-callback :pointer))

(defun qlist-function-name (type-name name)
  (alexandria:symbolicate "SW_QLIST_"
                          (let* ((name (string-upcase type-name))
                                (colons (search "::" name)))
                            (if colons
                                (subseq name (+ colons 2))
                                name))
                          "_"
                          (string-upcase name)))

(macrolet ((define-qlist-marshaller-funcs (type-name)
               (flet ((func-name (name)
                        (concatenate 'string "sw_qlist_" (string-downcase type-name)
                                     "_" (string-downcase name))))
                 `(progn
                    (defcfun ,(func-name "new") :pointer)
                    (defcfun ,(func-name "delete") :void (qlist :pointer))
                    (defcfun ,(func-name "size") :int (qlist :pointer))
                    (defcfun ,(func-name "at") :pointer (qlist :pointer) (index :int))
                    (defcfun ,(func-name "append") :void (qlist :pointer) (var :pointer))))))
  (define-qlist-marshaller-funcs void)
  (define-qlist-marshaller-funcs int)
  (define-qlist-marshaller-funcs papersize)
  (define-qlist-marshaller-funcs qvariant)
  (define-qlist-marshaller-funcs qbytearray)
  (define-qlist-marshaller-funcs qmodelindex)
  (define-qlist-marshaller-funcs qkeysequence)
  (define-qlist-marshaller-funcs extraselection))

(cffi:defcstruct SmokeData
  (name (:string :encoding :ascii))
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
  (binding :pointer))

(macrolet ((% (fun slot)
             `(progn
                (declaim (inline ,fun))
                (defun ,fun (data)
                  (cffi:foreign-slot-value data '(:struct SmokeData) ',slot)))))
  (% data-name name)
  (% data-classes classes)
  (% data-nclasses nclasses)
  (% data-methods methods)
  (% data-nmethods nmethods)
  (% data-methodmaps methodmaps)
  (% data-nmethodmaps nmethodmaps)
  (% data-methodnames methodnames)
  (% data-nmethodnames nmethodnames)
  (% data-types types)
  (% data-ntypes ntypes)
  (% data-inheritanceList inheritanceList)
  (% data-argumentList argumentList)
  (% data-ambiguousMethodList ambiguousMethodList)
  (% data-castFn castFn)
  (% data-binding binding))

(cffi:defcunion StackItem
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

(cffi:defcstruct qMethod
  (classid :short)
  (name :short)
  (args :short)
  (numargs :unsigned-char)
  (flags :unsigned-short)
  (ret :short)
  (methodForClassFun :short))

(cffi:defcstruct MethodMap
  (classid :short)
  (name :short)
  (methodid :short))

(cffi:defcstruct qClass
  (className (:string :encoding :ascii))
  (external :char)
  (parents :short)
  (classfn :pointer) ;; void (*classfn)(Index method, void *obj, Stack args)
  (enumfn :pointer)
  (flags :short)
  (size :unsigned-int))

(cffi:defcstruct qType
  (name (:string :encoding :ascii))
  (classid :short)
  (flags :short))

#+sbcl
(defvar *floating-point-mode* nil)

(defmacro with-fp-traps-masked (&body body)
  `(let (#+sbcl
         (*floating-point-mode* (sb-vm:floating-point-modes)))
     (#+sbcl sb-int:with-float-traps-masked
      #+sbcl (:invalid :divide-by-zero :underflow :overflow :inexact)
      #-sbcl progn
      ,@body)))

(defmacro with-fp-traps-restored (&body body)
  #+sbcl
  (let ((current (gensym "CURRENT")))
   `(let ((,current (sb-vm:floating-point-modes)))
      (unwind-protect
           (progn (when *floating-point-mode*
                    (setf (sb-vm:floating-point-modes)
                          *floating-point-mode*))
                  ,@body)
        (setf (sb-vm:floating-point-modes) ,current))))
  #-sbcl
  `(progn ,@body))

(defvar *callbacks* (make-hash-table :test 'equal))

(defmacro defcallback (name ret (&rest args) &body body)
  `(cffi:defcallback (,name #+commonqt-use-stdcall :calling-convention
                            #+commonqt-use-stdcall :stdcall)
       ,ret ,args ,@body))

(defcallback deletion-callback
    :void
    ((obj :pointer))
  ;; Just dispatch to an ordinary function for debugging purposes.
  ;; Redefinition of a callback wouldn't affect the existing C++ code,
  ;; redefinition of the function does.
  (%deletion-callback obj))

(defcallback method-invocation-callback
    :char
    ((smoke :pointer)
     (method :short)
     (obj :pointer)
     (args :pointer))
  (with-fp-traps-restored
    (%method-invocation-callback smoke method obj args)))

(defcallback dynamic-invocation-callback
    :char
    ((smoke :pointer)
     (method :short)
     (override-id :short)
     (obj :pointer)
     (args :pointer))
  (with-fp-traps-restored
    (%dynamic-invocation-callback smoke obj method override-id args)))

(defcallback metacall-callback
    :void
    ((obj :pointer)
     (id :int)
     (slot-or-signal-id :int)
     (args :pointer))
  (with-fp-traps-restored
    (qt_metacall-override obj id slot-or-signal-id args)))

(defvar *ptr-callback*)

(defcallback ptr-callback
    :void
    ((obj :pointer))
  (funcall *ptr-callback* obj))
