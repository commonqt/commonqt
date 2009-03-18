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

(defmethod unmarshal-using-type ((kind (eql :pointer))
                                 (name t)
                                 (stack-item-slot (eql 'class))
                                 type
                                 stack-item)
  (%qobject (qtype-class type)
            (cffi:foreign-slot-value stack-item
                                     '|union StackItem|
                                     'class)))

(defmethod unmarshal-using-type ((kind (eql :stack))
                                 (name t)
                                 (stack-item-slot (eql 'class))
                                 type
                                 stack-item)
  (%qobject (qtype-class type)
            (cffi:foreign-slot-value stack-item
                                     '|union StackItem|
                                     'class)))

(defmethod unmarshal-using-type ((kind (eql :pointer))
                                 (name (eql :|const char*|))
                                 (stack-item-slot (eql 'ptr))
                                 type
                                 stack-item)
  (cffi:foreign-string-to-lisp
   (cffi:foreign-slot-value stack-item
                            '|union StackItem|
                            'ptr)))

(defmethod unmarshal-using-type ((kind (eql :pointer))
                                 (name (eql :|char*|))
                                 (stack-item-slot (eql 'ptr))
                                 type
                                 stack-item)
  (cffi:foreign-string-to-lisp
   (cffi:foreign-slot-value stack-item
                            '|union StackItem|
                            'ptr)))

(defmethod unmarshal-using-type ((kind (eql :pointer))
                                 (name (eql :|void**|))
                                 (stack-item-slot (eql 'ptr))
                                 type
                                 stack-item)
  (void** (cffi:foreign-slot-value stack-item '|union StackItem| 'ptr)))

(defmethod unmarshal-using-type ((kind (eql :stack))
                                 (name (eql :|int|))
                                 (stack-item-slot (eql 'int))
                                 type
                                 stack-item)
  ;; was: (int ...)
  (cffi:foreign-slot-value stack-item
                                '|union StackItem|
                                'int))

(defmethod unmarshal-using-type ((kind (eql :stack))
                                 (name (eql :|double|))
                                 (stack-item-slot (eql 'double))
                                 type
                                 stack-item)
  (cffi:foreign-slot-value stack-item
                                '|union StackItem|
                                'double))

(defmethod unmarshal-using-type ((kind (eql :stack))
                                 (name (eql :|float|))
                                 (stack-item-slot (eql 'float))
                                 type
                                 stack-item)
  (cffi:foreign-slot-value stack-item
                                '|union StackItem|
                                'float))

(defmethod unmarshal-using-type ((kind (eql :stack))
                                 (name (eql :|bool|))
                                 (stack-item-slot (eql 'bool))
                                 type
                                 stack-item)
  (logbitp 0 (cffi:foreign-slot-value stack-item
                                      '|union StackItem|
                                      'int)))

(defmethod unmarshal-using-type ((kind (eql :stack))
                                 name
                                 (stack-item-slot (eql 'enum))
                                 type
                                 stack-item)
  (enum (cffi:foreign-slot-value stack-item
                                '|union StackItem|
                                'enum)
        (qtype-interned-name type)))

(defmethod unmarshal-using-type ((kind (eql :stack))
                                 (name (eql :|QString|))
                                 (stack-item-slot (eql 'ptr))
                                 type
                                 stack-item)
  ;; fixme: memory leak
  (call (%qobject (find-qclass "QByteArray")
                  (sw_qstring_to_utf8
                   (cffi:foreign-slot-value stack-item
                                            '|union StackItem|
                                            'ptr)))
        "data"))
