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

(defun interpret-delete (object)
  (cond
    ((typep object 'null-qobject)
     (error "Cannot delete null object: ~A" object))
    ((qobject-deleted object)
     (warn "Not deleting dead object: ~A" object))
    (t
     (optimized-call nil object (resolve-delete object))
     (note-deleted object))))

#+nil
(defun resolve-delete (object)
  (let ((dtor (format nil "~~~A" (qclass-name (qobject-class object)))))
    (lambda (object)
      (cond
        ((typep object 'null-qobject)
         (error "Cannot delete null object: ~A" object))
        ((qobject-deleted object)
         (warn "not deleting dead object: ~A" object))
        (t
         (optimized-call nil object dtor)
         (note-deleted object))))))

(defun resolve-delete (object)
  ;; (format *trace-output* "cache miss for #_delete ~A~%" object)
  (format nil "~~~A" (qclass-name (qobject-class object))))

(defmacro optimized-delete (object)
  `(let ((object ,object))
     (cached-values-bind (dtor) (resolve-delete object)
         (((qobject-class object) :hash t))
       (cond
         ((typep object 'null-qobject)
          (error "Cannot delete null object: ~A" object))
         ((qobject-deleted object)
          (warn "Not deleting dead object: ~A" object))
         (t
          (optimized-call nil object dtor)
          (note-deleted object))))))

(defvar *inhibit-caching* nil)

(defun cache! (object)
  (unless *inhibit-caching*
    (let ((ptr (qobject-pointer object)))
      ;; (assert (null (pointer->cached-object ptr)))
      (setf (pointer->cached-object ptr) object)
      (assert (qobject-class object))
      (map-casted-object-pointer
       (lambda (super-ptr)
         (unless (cffi:pointer-eq super-ptr ptr)
           (setf (pointer->cached-object super-ptr) object)))
       (qobject-class object)
       ptr)))
  object)

(defun get-slot-or-symbol (qt-class id)
  (let ((table (slot-or-signal-table qt-class)))
    (when (< id (length table))
      (elt table id))))

(defun inform-cpp-about-override (qclass binding method-name
                                  override-id)
  (let (once
        (original-module (ldb-module qclass)))
    (flet ((inform (class)
             (map-class-methods-named
              (lambda (<method>)
                (setf once t)
                (sw_override binding
                             (unbash* <method> +method+)
                             override-id))
              class method-name)
             (unless (eql original-module (ldb-module class))
               (map-method-in-class-module
                (lambda (<method>)
                  (setf once t)
                  (sw_override binding
                               (unbash* <method> +method+)
                               override-id))
                (find-qclass-in-module original-module (qclass-name class))
                method-name))))
      (map-qclass-precedence-list #'inform qclass))
    (unless once
      (warn "~a has no method named ~s, can't override it."
            (qclass-name qclass)
            method-name))))

(defun inform-cpp-about-overrides (qt-class)
  (let ((<class> (slot-value qt-class 'effective-class))
        (binding (class-binding qt-class)))
    (loop for spec in (class-overrides qt-class)
          for id from 0
          do
          (inform-cpp-about-override <class> binding (name spec) id))))

(defun meta-object-method-index (qt-class)
  (map-class-methods-named
   (lambda (<method>)
     (return-from meta-object-method-index
       (values (unbash* <method> +method+))))
   (slot-value qt-class 'effective-class)
   "metaObject"))

(defun metacall-method-index (qt-class)
  (map-class-methods-named
   (lambda (<method>)
     (return-from metacall-method-index
       (values (unbash* <method> +method+))))
   (slot-value qt-class 'effective-class)
   "qt_metacall"))

(defun set-class-binding (qt-class)
  (multiple-value-bind (idx <module>)
      (unbash* (slot-value qt-class 'effective-class) +class+)
    (declare (ignore idx))
    (setf (class-binding qt-class)
          (sw_make_dynamic_binding (module-ref <module>)
                                   (qobject-pointer
                                    (slot-value qt-class 'qmetaobject))
                                   (meta-object-method-index qt-class)
                                   (metacall-method-index qt-class)
                                   (cffi:callback deletion-callback)
                                   (cffi:callback dynamic-invocation-callback)
                                   (cffi:callback metacall-callback)))))

(defun ensure-qt-class-caches (qt-class)
  (check-type qt-class qt-class)
  (with-slots (effective-class qmetaobject smoke-generation generation)
      qt-class
    (unless (and qmetaobject
		 effective-class
		 (eq smoke-generation *cached-objects*))
      ;; clear everything out to ensure a clean state in case of errors
      ;; in the following forms
      (setf effective-class nil)
      (setf qmetaobject nil)
      (setf smoke-generation nil)
      ;; reinitialize things
      (setf effective-class (find-qclass
                             (class-qt-superclass qt-class)))
      (mapc #'initialize-slot-or-signal
            (class-signals qt-class))
      (mapc #'initialize-slot-or-signal
            (class-slots qt-class))
      (setf qmetaobject
            (let* ((class (find-qclass
                           (class-qt-superclass qt-class)))
                   (qobject-class (find-qclass "QObject"))
                   (parent (cond
                             ((qsubclassp class qobject-class)
                              (#_staticMetaObject class))
                             (t
                              (null-qobject (find-qclass "QMetaObject"))))))
              (make-metaobject parent
                               (let ((name (class-name qt-class)))
                                 (format nil "~A::~A"
                                         (package-name (symbol-package name))
                                         (symbol-name name)))
                               (class-class-infos qt-class)
                               (class-signals qt-class)
                               (class-slots qt-class))))
      (set-class-binding qt-class)
      (inform-cpp-about-overrides qt-class)
      ;; invalidate call site caches
      (setf generation (gensym))
      ;; mark as fresh
      (setf (class-smoke-generation qt-class) *cached-objects*))))

(defun class-effective-class (qt-class &optional (errorp t))
  (ensure-qt-class-caches qt-class)
  (or (slot-value qt-class 'effective-class)
      (when errorp
        (error "effective-class not cached?"))))

(defun qobject-metaobject (object)
  (if (typep object 'dynamic-object)
      (class-qmetaobject (class-of object))
      (#_metaObject object)))

(defun class-qmetaobject (qt-class)
  (ensure-qt-class-caches qt-class)
  (slot-value qt-class 'qmetaobject))

(defun find-method-override (object method)
  (if (typep object 'dynamic-object)
      (find-method-override-using-class (class-of object) method)
      nil))

(defun find-dynamic-method-override (object method-id)
  (svref (override-table (class-of object)) method-id))

(defun find-method-override-using-class (class method)
  (let ((table (lisp-side-override-table class)))
    (when table
      (gethash (qmethod-name method) table))))

(defvar *next-qmethod-trampoline* nil)
(defvar *next-qmethod* nil)

(defun call-next-qmethod (&rest args)
  (unless *next-qmethod-trampoline*
    (error "~a used outside of overriding method."
           'call-next-qmethod))
  (funcall *next-qmethod-trampoline* args))

(defun stop-overriding ()
  "Like call-next-qmethod, but doesn't return.
Should be used as an optimization."
  (unless *next-qmethod-trampoline*
    (error "~a used outside of overriding method."
           'stop-overriding))
  (throw 'stop-overriding-tag 0))

(defun get-next-qmethod ()
  (or *next-qmethod*
      (error "get-next-qmethod used outside of overriding method")))

(defun override (fun object method-name args)
  (let ((*next-qmethod* method-name)
        (*next-qmethod-trampoline*
          (lambda (new-args)
            (apply #'interpret-call-without-override
                   object
                   method-name
                   (or new-args args)))))
    (apply fun object args)))

(defgeneric dynamic-slot-or-signal (object id)
  (:method (object id)
    (declare (ignore object id))
    nil))

(defun qt_metacall-override (object-ptr id slot-signal-index stack)
  (with-callback-restart
    (let* ((object (pointer->cached-object object-ptr))
           (member
             (or
              (get-slot-or-symbol (class-of object) slot-signal-index)
              (dynamic-slot-or-signal object slot-signal-index)
              (error "QT_METACALL-OVERRIDE: invalid member id ~A"
                     slot-signal-index))))
      (etypecase member
        (signal-spec
         (#_activate (class-qmetaobject (class-of object))
                     object
                     id
                     stack))
        (slot-spec
         (apply (spec-function member)
                object
                (unmarshal-slot-args member stack)))))))

(defun unmarshal-slot-args (member argv)
  (iter (for type in (arg-qtypes member))
        (for i from 1)
        (collect (cond ((eq (qtype-interned-name type) ':|QString|)
                        (qstring-pointer-to-lisp
                         (cffi:mem-aref argv :pointer i)))
                       ((and
                         (eq (qtype-kind type) :stack)
                         (eq (qtype-stack-item-slot type) 'class))
                        (unmarshal type (cffi:mem-aptr argv :pointer i)))
                       (t
                        (unmarshal type (cffi:mem-aref argv :pointer i)))))))

(defun process-slot-signal-signature (signature)
  (ppcre:register-groups-bind (reply-type name types)
      ("^(?:([\\w,<>:]*)\\s+)?([^\\s]*)\\((.*)\\)"
       (#_data (#_QMetaObject::normalizedSignature signature)))
    (return-from ;; OR cannot be used because of multiple values
     process-slot-signal-signature
      (values (concatenate 'string name "(" types ")")
              types
              (if (or (null reply-type)
                      (equal reply-type "void"))
                  ""
                  reply-type))))
  (error "Invalid slot or signal signature: ~s" signature))

(defparameter *signal-type-translation*
  '(("uint" . "unsigned int")
    ("ulong" . "unsigned long")))

(defun find-signal-qtype (name)
  (let ((name (or (cdr (assoc name *signal-type-translation*
                              :test #'equal))
                  name)))
    (or (find-qtype name)
        (error "No smoke type found for dynamic member arg type ~A.  Giving up."
               name))))

(defun find-signal-arg-qtypes (arg-types)
  (mapcar #'find-signal-qtype (cl-ppcre:split "," arg-types)))

(defun initialize-slot-or-signal (slot-or-signal)
  (unless (full-name slot-or-signal)
    (multiple-value-bind (full-name arg-types reply-type)
        (process-slot-signal-signature (name slot-or-signal))
      (setf (full-name slot-or-signal) full-name
            (arg-types slot-or-signal) arg-types
            (reply-type slot-or-signal) reply-type
            (arg-qtypes slot-or-signal) (find-signal-arg-qtypes arg-types))))
  slot-or-signal)

(defconstant +AccessPrivate+ #x00)
(defconstant +AccessProtected+ #x01)
(defconstant +AccessPublic+ #x02)
(defconstant +MethodMethod+ #x00)
(defconstant +MethodSignal+ #x04)
(defconstant +MethodSlot+ #x08)
(defconstant +MethodCompatibility+ #x10)
(defconstant +MethodCloned+ #x20)
(defconstant +MethodScriptable+ #x40)

(defun make-metaobject-signature (class-name class-infos signals slots)
  (let ((table (make-hash-table :test #'equal))
        data)
    (values
     (with-output-to-string (stream)
       (labels ((intern-string (s)
                  (or (gethash s table)
                      (setf (gethash s table)
                            (prog1
                                (file-position stream)
                              (write-string s stream)
                              (write-char (code-char 0) stream)))))
                (add (x) (push x data))
                (add-string (s) (add (intern-string s))))
         (add 1)                          ;revision
         (add (intern-string class-name)) ;class name
         (add (length class-infos))       ;classinfo
         (add (if (plusp (length class-infos)) 10 0))
         (add (+ (length signals) (length slots)))
         (add (+ 10 (* 2 (length class-infos)))) ;methods
         (add 0)                                 ;properties
         (add 0)
         (add 0)                        ;enums/sets
         (add 0)
         (dolist (entry class-infos)
           (add-string (key entry))
           (add-string (value entry)))
         (dolist (entry signals)
           (add-string (full-name entry))
           (add-string (remove #\, (full-name entry) :test-not #'eql))
           (add-string (reply-type entry))
           (add-string "")              ;tag
           (add (logior +methodsignal+ +accessprotected+)))
         (dolist (entry slots)
           (add-string (full-name entry))
           (add-string (remove #\, (full-name entry) :test-not #'eql))
           (add-string (reply-type entry))
           (add-string "")              ;tag
           (add (logior +methodslot+ +accesspublic+)))
         (add 0)))
     (nreverse data))))

(defun make-metaobject (parent class-name class-infos signals slots)
  (multiple-value-bind (signature data)
      (make-metaobject-signature class-name
                                 class-infos signals slots)
    (let ((dataptr (cffi:foreign-alloc :int :count (length data))))
      (loop for x in data
            for i from 0
            do
            (setf (cffi:mem-aref dataptr :int i) x))
      (make-instance 'qobject
                     :class (find-qclass "QMetaObject")
                     :pointer
                     (sw_make_metaobject (qobject-pointer parent)
                                         (cffi:foreign-string-alloc signature :encoding :ascii)
                                         dataptr)))))
