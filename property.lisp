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

(defun object-properties (object)
  (metaobject-properties (#_metaObject object)))

(defun metaobject-properties (meta &optional (include-inherited t))
  (let ((count (#_propertyCount meta))
        (offset (if include-inherited
                    0
                    (#_propertyOffset meta))))
    (iter (for i from offset below count)
          (collect (#_property meta i)))))

(defparameter *property-types* (make-hash-table :test 'equal))

(defmacro defpropertytype (name (object-var) &body body)
  `(setf (gethash ',name *property-types*)
         (lambda (,object-var) ,@body)))

(defpropertytype "int" (p) (#_toInt p))
(defpropertytype "uint" (p) (#_toUInt p))
(defpropertytype "float" (p) (#_toFloat p))
(defpropertytype "double" (p) (#_toDouble p))
(defpropertytype "bool" (p) (#_toBool p))

(defpropertytype "QString" (p) (#_toString p))
(defpropertytype "QLocale" (p) (#_toLocale p))
(defpropertytype "QSize" (p) (#_toSize p))
(defpropertytype "QPoint" (p) (#_toPoint p))
(defpropertytype "QRect" (p) (#_toRect p))

(defun variant-reader (type-name)
  (or (gethash type-name *property-types*)
      (progn
        (warn "no reader known for property type ~S, returning variant"
              type-name)
        #'identity)))

(defun property (object property)
  (multiple-value-bind (name property)
      (etypecase property
        (qobject (values (#_name property) property))
        (string (values property (or (find property
                                           (object-properties object)
                                           :key (lambda (x) (#_name x))
                                           :test #'equal)
                                     (error "no such property ~A on ~A"
                                            property object)))))
    (let ((variant (#_property object name)))
      (if variant
          (values (funcall (variant-reader (#_typeName property)) variant) t)
          (values nil nil)))))

(defun describe-qobject (thing)
  (format t "~A is a smoke object.~%~%" thing)
  (format t "Use (~S ~S) to see details about its C++ class.~%~%"
          'qdescribe (qclass-name (qobject-class thing)))
  (when (typep thing 'dynamic-object)
    (format t "This object is also an instance of a Lisp class, ~A.~%~%"
            (class-name (class-of thing))))
  (when (qtypep thing (qt::find-qclass "QObject"))
    (format t "Properties~A:~%"
            (if (typep thing 'dynamic-object) " and slots" ""))
    (dolist (prop (object-properties thing))
      (multiple-value-bind (value boundp)
          (handler-bind ((warning #'muffle-warning))
            (property thing prop))
        (format t "~4T~A ~A~40T"
                (#_typeName prop)
                (#_name prop))
        (if boundp
            (format t "~S~%" value)
            (format t "<unbound>~%")))))
  (when (typep thing 'dynamic-object)
    (dolist (slotd (c2mop:class-slots (class-of thing)))
      (let ((name (c2mop:slot-definition-name slotd)))
        (unless (member name '(class pointer))
          (format t "~4T~A~40T" name)
          (if (slot-boundp thing name)
              (format t "~S~%" (slot-value thing name))
              (format t "<unbound>~%")))))))

(defun describe-qclass-properties (class inherited)
  (when (qclass-find-applicable-method class "staticMetaObject" nil nil)
    (let* ((meta (#_staticMetaObject class))
           (super (#_superClass meta)))
      (format t "~%Properties:~%")
      (dolist (prop (metaobject-properties meta nil))
        (format t "~4T~A ~A~40T~%" (#_typeName prop) (#_name prop)))
      (when super
        (cond
          (inherited
           (format t "~%Inherited properties:~%")
           (dolist (prop (metaobject-properties super t))
             (format t "~4T~A ~A~40T~%" (#_typeName prop) (#_name prop))))
          (t
           (format t "~%Use (QDESCRIBE ~S T) to see inherited properties.~%"
                   (qclass-name class))))))))
