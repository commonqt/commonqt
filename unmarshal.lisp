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

;;; (defun stack-item-accessor (slot)
;;;   ;; returns a lambda that calls CFFI:FOREIGN-SLOT-VALUE, except that the
;;;   ;; latter is slow when called with a non-constant slot name, so we
;;;   ;; dispatch on the slot name to return a closure optimized for each case.
;;;   (macrolet
;;;       ((% ()
;;;          `(ecase slot
;;;             ,@ (mapcar (lambda (slot)
;;;                          `((,slot)
;;;                            (lambda (stack-item)
;;;                              (cffi:foreign-slot-value stack-item
;;;                                                       '|union StackItem|
;;;                                                       ',slot))))
;;;                        '(ptr bool char uchar short ushort int
;;;                          uint long ulong float double enum class)))))
;;;     (%)))

(defmacro dispatching-on-stack-item ((getter slot) &body body)
  ;; returns a lambda that calls CFFI:FOREIGN-SLOT-VALUE, except that the
  ;; latter is slow when called with a non-constant slot name, so we
  ;; dispatch on the slot name to return a closure optimized for each case.
  `(ecase ,slot
     ,@ (mapcar (lambda (slot)
                  `((,slot)
                    (macrolet
                        ((,getter (stack-item)
                           `(cffi:foreign-slot-value ,stack-item
                                                     '|union StackItem|
                                                     ',',slot)))
                      ,@body)))
                '(ptr bool char uchar short ushort int
                  uint long ulong float double enum class))))

(defun unmarshal-using-type (type stack-item)
  (funcall (unmarshaller type) stack-item))

(defun unmarshaller (type)
  (if (qtype-void-p type)
      (constantly nil)
      (let ((thunk (unmarshaller-2 type)))
        (dispatching-on-stack-item (get-value (qtype-stack-item-slot type))
          (lambda (stack-item)
            (funcall thunk (get-value stack-item) type))))))

(defun unmarshaller-2 (type)
  (let ((name (qtype-name type)))
    (or (get-static-unmarshaller name)
        (case (qtype-stack-item-slot type)
          (class (lambda (value type) (%qobject (qtype-class type) value)))
          (enum  (lambda (value type) (enum value (qtype-interned-name type))))
          (t    (or (get-dynamic-unmarshaller name)
                    (lambda (value type)
                      (declare (ignore type))
                      value)))))))

(defvar *static-unmarshallers* (make-hash-table :test #'equal))

(defun get-static-unmarshaller (name)
  (gethash name *static-unmarshallers*))

(defmacro def-unmarshal ((var name type) &body body)
  `(setf (gethash ,name *static-unmarshallers*)
         (fdefinition
          (defun ,(intern (format nil "~a-~a" name 'unmarshaller))
              (,var ,type)
            (declare (ignorable ,type))
            ,@body))))

(def-unmarshal (value "const char*" type)
  (cffi:foreign-string-to-lisp value))

(def-unmarshal (value "char*" type)
  (cffi:foreign-string-to-lisp value))

(def-unmarshal (value "void**" type)
  value)

(def-unmarshal (value "bool" type)
  (logbitp 0 value))

(def-unmarshal (value "QString" type)
  (qstring-pointer-to-lisp value))

(def-unmarshal (value "QThread*" type)
  (make-instance 'qthread :pointer value))

(def-unmarshal (value "QVariant" type)
  (unvariant value type))

(def-unmarshal (value "QByteArray" type)
  (interpret-call (%qobject (find-qclass "QByteArray") value) "data"))

(defvar *dynamic-unmarshallers* nil)

(defun get-dynamic-unmarshaller (name)
  (loop for (nil (test unmarshaller-maker)) on *dynamic-unmarshallers* by #'cddr
        when (funcall test name)
        return
        (let ((marshaller (funcall unmarshaller-maker name)))
          (setf (gethash name *static-unmarshallers*)
                marshaller))))

(defun set-dynamic-unmarshal (name test maker)
  (setf (getf *dynamic-unmarshallers* name)
        (list test maker)))

(set-dynamic-unmarshal
 'qlist
 (lambda (name) (alexandria:starts-with-subseq "QList<" name))
 'unmarshal-qlist)
