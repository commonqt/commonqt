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
(named-readtables:in-readtable :qt)

(defun resolve-cast (<from> <to>)
  (let* ((module (ldb-module <from>))
         (compatible-<to>
          (if (eql module (ldb-module <to>))
              <to>
              (find-qclass-in-module module (qclass-name <to>)))))
    (unless compatible-<to>
      (error "sorry, casting across modules in several steps not yet supported"))
    (values (data-castfn (data-ref module))
            compatible-<to>)))

(defun perform-cast (obj castfn <from> <to>)
  (cffi:foreign-funcall-pointer
   castfn
   ()
   :pointer (qobject-pointer obj)
   :short (unbash <from>)
   :short (unbash <to>)
   :pointer))

(defun %cast (obj <to>)
;;;   (let* ((from-class (qobject-class obj))
;;;          (module (ldb-module from-class))
;;;          (to-class
;;;           (if (eql module (ldb-module to-class))
;;;               to-class
;;;               (find-qclass-in-module module (qclass-name to-class)))))
;;;     (cffi:foreign-funcall-pointer
;;;      (data-castfn (data-ref module))
;;;      ()
;;;      :pointer (qobject-pointer obj)
;;;      :short (unbash from-class)
;;;      :short (unbash to-class)
;;;      :pointer))
  (let ((<from> (qobject-class obj)))
    (multiple-value-bind (fn <cto>)
        (resolve-cast <from> <to>)
      (perform-cast obj fn <from> <cto>))))

(defun marshal (value type stack-item cont)
  (funcall (marshaller value type) value stack-item cont))

(defun marshaller (obj <type>)
  (let* ((set-thunk
          (macrolet
              ((si (slot)
                 `(cffi:foreign-slot-value si '|union StackItem| ',slot))
               (dispatching ((getter slot) &body body)
                 `(ecase ,slot
                    ,@ (mapcar (lambda (slot)
                                 `((,slot)
                                   (macrolet ((,getter () `(si ,',slot)))
                                     ,@body)))
                               '(ptr bool char uchar short ushort int
                                 uint long ulong float double enum class)))))
            (let ((slot (qtype-stack-item-slot <type>)))
              (case slot
                (bool  (lambda (val si) (setf (si bool) (if val 1 0))))
                (class (if (typep obj 'qobject)
                           (let ((<from> (qobject-class obj)))
                             (multiple-value-bind (castfn <to>)
                                 (resolve-cast <from> (qtype-class <type>))
                               (lambda (val si)
                                 (setf (si class)
                                       (perform-cast val castfn <from> <to>)))))
                           (lambda (val si)
                             (setf (si class)
                                   (if (typep val 'cffi:foreign-pointer)
                                       val
                                       (qobject-pointer val))))))
                (enum (etypecase obj
                        (integer
                         (lambda (val si) (setf (si enum) val)))
                        (enum
                         (lambda (val si) (setf (si enum) (primitive-value val))))))
                (int (etypecase obj
                       (integer
                        (lambda (val si) (setf (si int) val)))
                       (enum
                        (lambda (val si) (setf (si int) (primitive-value val))))))
                (uint (etypecase obj
                        (integer
                         (lambda (val si) (setf (si uint) val)))
                        (enum
                         (lambda (val si) (setf (si uint) (primitive-value val))))))
                (float (lambda (val si) (setf (si float) (float val 1.0s0))))
                (double (lambda (val si) (setf (si double) (float val 1.0d0))))
                ;; that leaves:
                ;;   ptr char uchar short ushort int uint long ulong
                (t
                 (dispatching (%si slot)
                              (lambda (val si)
                                (setf (%si) val))))))))
         (primary-cons
          (get (qtype-interned-name <type>) 'marshaller/primary))
         (around-cons
          (get (qtype-interned-name <type>) 'marshaller/around))
         (primary-type (car primary-cons))
         (primary-thunk (cdr primary-cons))
         (around-type (car around-cons))
         (around-thunk (cdr around-cons)))
    (cond
      ((and primary-thunk (typep obj primary-type))
       (assert (null around-thunk))
       (named-lambda marshal-primary-outer (value stack-item cont)
         (funcall set-thunk
                  (funcall primary-thunk value)
                  stack-item)
         (funcall cont)))
      ((and around-thunk (typep obj around-type))
       (named-lambda marshal-around-outer (value stack-item cont)
         (funcall around-thunk
                  value
                  (named-lambda marshal-around-inner (new-value)
                    (funcall set-thunk new-value stack-item)
                    (funcall cont)))))
      (t
       (named-lambda marshal-default (value stack-item cont)
         (funcall set-thunk value stack-item)
         (funcall cont))))))

(defmacro defmarshal ((var name &key around (type t)) &body body)
  (if (consp name)
      `(progn
         ,@(iter (for n1 in name)
                 (collect `(defmarshal (,var ,n1 :around ,around :type ,type) ,@body))))
      (let ((function-name (intern (format nil "~a-~a" name 'marshaller))))
        (if around
            `(setf (get ',name 'marshaller/primary) nil
                   (get ',name 'marshaller/around)
                   (cons ',type (named-lambda ,function-name (,var ,around) ,@body)))
            `(setf (get ',name 'marshaller/primary)
                   (cons ',type (named-lambda ,function-name (,var) ,@body))
                   (get ',name 'marshaller/around) nil)))))

(defmarshal (value (:|QString| :|const QString&|) :around cont :type string)
  (let ((qstring (sw_make_qstring value)))
    (unwind-protect
         (funcall cont qstring)
      (sw_delete_qstring qstring))))

;;; Don't delete the string because it may be used afterwards by Qt
(defmarshal (value :|QString*| :around cont :type string)
  (funcall cont (sw_make_qstring value)))

(defmarshal (value :|const char*| :around cont :type string)
  (let ((char* (cffi:foreign-string-alloc value)))
    (unwind-protect
         (funcall cont char*)
      (cffi:foreign-free char*))))

(defmarshal (value :|unsigned char*| :around cont :type string)
  (let ((char* (cffi:foreign-string-alloc value)))
    (unwind-protect
         (funcall cont char*)
      (cffi:foreign-free char*))))

(defmarshal (value (:|QByteArray| :|const QByteArray&|) :around cont :type string)
  (let ((qbytearray (sw_make_qbytearray value)))
    (unwind-protect
         (funcall cont qbytearray)
      (sw_delete_qbytearray qbytearray))))
