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
(named-readtables:in-readtable :qt)

(defun %cast (ptr from to)
  (cffi:foreign-funcall-pointer
   *castfn*
   ()
   :pointer ptr
   :short from
   :short to
   :pointer))

(defmarshal (t t (eql 'class))
    ((argument abstract-qobject)
     type
     stack-item
     :test #'qtypep)
  (setf (cffi:foreign-slot-value stack-item
                                 '|union StackItem|
                                 (qtype-stack-item-slot type))
        ;; Need to cast the C++ object explicitly, because casting in
        ;; the presence of multiple inheritance isn't just cosmetics,
        ;; it does pointer arithmetic.
        (%cast (qobject-pointer argument)
               (qclass-id (qobject-class argument))
               (qclass-id (qtype-class type))))
  (marshal-next))

(defmarshal ((eql :reference) (eql :|int&|) t)
    ((argument $)
     type
     stack-item)
  (cffi:with-foreign-object (reference :int)
    (setf (cffi:mem-aref reference :int) (primitive-value argument))
    (setf (cffi:foreign-slot-value stack-item
                                   '|union StackItem|
                                   (qtype-stack-item-slot type))
          reference)
    (splice-reference-result
     (marshal-next)
     (cffi:mem-aref reference :int))))

(defmarshal ((eql :reference) (eql :|int&|) t)
    ((argument int&)
     type
     stack-item)
  (setf (cffi:foreign-slot-value stack-item
                                 '|union StackItem|
                                 (qtype-stack-item-slot type))
        (primitive-value argument))
  (splice-reference-result
   (marshal-next)
   (cffi:mem-aref (primitive-value argument) :int)))

(defmarshal ((eql :reference) (eql :|const QString&|) (eql 'ptr))
    ((argument qstring)
     type
     stack-item)
  (let ((qstring (sw_make_qstring (primitive-value argument))))
    (setf (cffi:foreign-slot-value stack-item
                                   '|union StackItem|
                                   (qtype-stack-item-slot type))
          qstring)
    (unwind-protect
         (marshal-next)
      (sw_delete_qstring qstring))))

(defmarshal ((eql :reference) (eql :|const QString&|) (eql 'ptr))
    ((argument string)
     type
     stack-item)
  (let ((qstring (sw_make_qstring argument)))
    (setf (cffi:foreign-slot-value stack-item
                                   '|union StackItem|
                                   (qtype-stack-item-slot type))
          qstring)
    (unwind-protect
         (marshal-next)
      (sw_delete_qstring qstring))))

(defmarshal ((eql :pointer) (eql :|const char*|) (eql 'ptr))
    ((argument string)
     type
     stack-item)
  (let ((char* (cffi:foreign-string-alloc argument)))
    (setf (cffi:foreign-slot-value stack-item '|union StackItem| 'ptr)
          char*)
    (unwind-protect
         (marshal-next)
      (cffi:foreign-free char*))))

(defmarshal ((eql :pointer)
             t
             (eql 'ptr))
    ((argument ?)
     type
     stack-item)
  (setf (cffi:foreign-slot-value stack-item
                                 '|union StackItem|
                                 (qtype-stack-item-slot type))
        (primitive-value argument))
  (marshal-next))

(defmarshal ((eql :stack)
             (eql :|int|)
             (eql 'int))
    ((argument int)
     type
     stack-item)
  (setf (cffi:foreign-slot-value stack-item '|union StackItem| 'int)
        (primitive-value argument))
  (marshal-next))

(defmarshal ((eql :stack)
             (eql :|int|)
             (eql 'int))
    ((argument integer)
     type
     stack-item
     :test (lambda (arg *) (typep arg '(signed-byte 32))))
  (setf (cffi:foreign-slot-value stack-item '|union StackItem| 'int)
        argument)
  (marshal-next))

(defmarshal ((eql :stack)
             (eql :|uint|)
             (eql 'uint))
    ((argument uint)
     type
     stack-item)
  (setf (cffi:foreign-slot-value stack-item '|union StackItem| 'uint)
        (primitive-value argument))
  (marshal-next))

(defmarshal ((eql :stack)
             (eql :|uint|)
             (eql 'uint))
    ((argument integer)
     type
     stack-item
     :test (lambda (arg *) (typep arg '(unsigned-byte 32))))
  (setf (cffi:foreign-slot-value stack-item '|union StackItem| 'uint)
        argument)
  (marshal-next))

(defmarshal ((eql :stack)
             (eql :|unsigned int|)
             (eql 'uint))
    ((argument integer)
     type
     stack-item
     :test (lambda (arg *) (typep arg '(unsigned-byte 32))))
  (setf (cffi:foreign-slot-value stack-item '|union StackItem| 'uint)
        argument)
  (marshal-next))

(defmarshal ((eql :stack)
             (eql :|qreal|)
             (eql 'float))
    ((argument real)
     type
     stack-item)
  (setf (cffi:foreign-slot-value stack-item '|union StackItem| 'float)
        (float argument 1.0s0))
  (marshal-next))

(defmarshal ((eql :stack)
             (eql :|qreal|)
             (eql 'double))
    ((argument real)
     type
     stack-item)
  (setf (cffi:foreign-slot-value stack-item '|union StackItem| 'double)
        (float argument 1.0d0))
  (marshal-next))

(defmarshal ((eql :stack)
             (eql :|int|)
             (eql 'int))
    ((argument enum)
     type
     stack-item)
  (setf (cffi:foreign-slot-value stack-item '|union StackItem| 'int)
        (primitive-value argument))
  (marshal-next))

(defmarshal ((eql :stack)
             (eql :|bool|)
             (eql 'bool))
    ((argument bool)
     type
     stack-item)
  (setf (cffi:foreign-slot-value stack-item '|union StackItem| 'bool)
        (primitive-value argument))
  (marshal-next))

(defmarshal ((eql :stack)
             (eql :|bool|)
             (eql 'bool))
    ((argument t)
     type
     stack-item
     :test (lambda (arg *) (typep arg '(member t nil))))
  (setf (cffi:foreign-slot-value stack-item '|union StackItem| 'bool)
        (if argument 1 0))
  (marshal-next))

(defmarshal ((eql :stack)
             t
             (eql 'enum))
    ((argument enum)
     type
     stack-item
     :test (lambda (arg type)
             (eq (enum-type-name arg) (qtype-interned-name type))))
  (setf (cffi:foreign-slot-value stack-item '|union StackItem| 'enum)
        (primitive-value argument))
  (marshal-next))

(defmarshal ((eql :stack)
             t
             (eql 'uint))
    ((argument enum)
     type
     stack-item
     :test (lambda (arg type)
             t #+nil (eq (enum-type-name arg) (qtype-interned-name type))))
  (setf (cffi:foreign-slot-value stack-item '|union StackItem| 'uint)
        (primitive-value argument))
  (marshal-next))

(defmarshal ((eql :stack)
             t
             (eql 'enum))
    ((argument integer)
     type
     stack-item
     :test (lambda (arg *) (typep arg '(signed-byte 32))))
  (setf (cffi:foreign-slot-value stack-item '|union StackItem| 'enum)
        argument)
  (marshal-next))

(defmarshal ((eql :stack)
             (eql :|Qt::Alignment|)
             (eql 'uint))
    ((argument integer)
     type
     stack-item)
  (setf (cffi:foreign-slot-value stack-item '|union StackItem| 'uint)
        argument)
  (marshal-next))

(defmarshal ((eql :stack)
             (eql :|Qt::PenStyle|)
             (eql 'enum))
    ((argument enum)
     type
     stack-item
     :test (lambda (arg *)
             (or (eq (enum-type-name arg) :|Qt::GlobalColor|)
                 (eq (enum-type-name arg) :|Qt::PenStyle|))))
  (setf (cffi:foreign-slot-value stack-item '|union StackItem| 'uint)
        (primitive-value argument))
  (marshal-next))

;;; fixme: enum type safety? 

;;; (defmarshal ((eql :stack)
;;;              (eql :|QMetaObject::Call|)
;;;              (eql 'enum))
;;;     ((argument enum)
;;;      type
;;;      stack-item)
;;;   (setf (cffi:foreign-slot-value stack-item '|union StackItem| 'enum)
;;;         (primitive-value argument))
;;;   (marshal-next))

;;; (defmarshal ((eql :stack)
;;;              (eql :|QLCDNumber::SegmentStyle|)
;;;              (eql 'enum))
;;;     ((argument enum)
;;;      type
;;;      stack-item)
;;;   (setf (cffi:foreign-slot-value stack-item '|union StackItem| 'enum)
;;;         (primitive-value argument))
;;;   (marshal-next))

;;; (defmarshal ((eql :stack)
;;;              (eql |Qt::Orientation|)
;;;              (eql 'enum))
;;;     ((argument enum)
;;;      type
;;;      stack-item)
;;;   (setf (cffi:foreign-slot-value stack-item '|union StackItem| 'enum)
;;;         (primitive-value argument))
;;;   (marshal-next))

(defmarshal ((eql :pointer)
             (eql :|char**|)
             (eql 'ptr))
    ((argument vector)
     type
     stack-item)
  (loop
     for item across argument
     do (check-type item string))
  (cffi:with-foreign-object (argv :pointer (length argument))
    (string-vector-to-char**! argv argument)
    (multiple-value-prog1
        (marshal (char** argv)
                 type
                 stack-item
                 (lambda () (marshal-next)))
      (char**-to-string-vector! argument argv (length argument) t))))

(defmarshal ((eql :pointer) (eql :|bool*|) t)
    ((argument bool*)
     type
     stack-item)
  (setf (cffi:foreign-slot-value stack-item
                                 '|union StackItem|
                                 (qtype-stack-item-slot type))
        (primitive-value argument))
  (splice-reference-result
   (marshal-next)
   (cffi:mem-aref (primitive-value argument) :int)))
