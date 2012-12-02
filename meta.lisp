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

(defun postmortem (ptr class description qobjectp dynamicp)
  (declare (ignore ptr class))
  (format t "Finalizer called for ~A (~{~A~^, ~}), possible memory leak.~%"
          description
          (append (when dynamicp '("Lisp"))
                  (when qobjectp '("QObject"))))
  (force-output)
  #+(or)
  (let* ((object (%qobject class ptr))
         (parent (and qobjectp (#_parent object))))
    (cond
      ((or (not qobjectp)
           (and parent (null-qobject-p parent)))
       (format t "deleting ~A (~A)~%" object qobjectp)
       (force-output)
       (handler-case
           (if qobjectp
               (#_deleteLater object)
               (call object (format nil "~~~A" (qclass-name class))))
         (error (c)
           (format t "Error in finalizer: ~A, for object: ~A~%"
                   c description))))
      (dynamicp
       (warn "Bug in CommonQt?  previously dynamic object ~A still has parent ~A, but has been GCed"
             object parent))
      (t
       (warn "Bug in CommonQt?  ~A still has parent ~A; not deleting"
             object parent)))))

(defvar *report-memory-leaks* nil)

(defun cache! (object)
  (let ((ptr (qobject-pointer object)))
    ;; (assert (null (pointer->cached-object ptr)))
    (setf (pointer->cached-object ptr) object)
    (assert (qobject-class object))
    (map-casted-object-pointer
     (lambda (super-ptr)
       (unless (cffi:pointer-eq super-ptr ptr)
         (setf (pointer->cached-object super-ptr) object)))
     (qobject-class object)
     ptr)
    (when (typep object 'dynamic-object)
      (setf (gethash (cffi:pointer-address ptr) *strongly-cached-objects*)
            object)))
  (when (and *report-memory-leaks*
             (or (not (qtypep object (find-qclass "QObject")))
                 (typep (#_parent object) 'null-qobject)))
    (tg:finalize object
		 (let* ((ptr (qobject-pointer object))
			(class (qobject-class object))
			(str (princ-to-string object))
			(qobjectp (qsubclassp class (find-qclass "QObject")))
			(dynamicp (typep object 'dynamic-object)))
		   (lambda ()
		     (postmortem ptr class str qobjectp dynamicp)))))
  object)

(defun get-slot-or-symbol (qt-class id)
  (let ((table (slot-or-signal-table qt-class)))
    (when (< id (length table))
      (elt table id))))

(defun parse-function (form)
  ;; this run-time use of COMPILE is a huge kludge.  We'd just want to hook
  ;; into the DEFCLASS expansion like slots and init functions can, but
  ;; those are special built-in features of DEFCLASS which meta classes
  ;; cannot implement for their own options.  Big oversight in the MOP IMNSHO.
  (etypecase (macroexpand form)
    ((or symbol function)
     form)
    ((cons (eql lambda) t)
     (compile nil form))
    ((cons (eql function) t)
     (eval form))))

(defun compute-specs (class slot spec-class
                      raw-specs)
  (let* ((result
           (loop for (name . value) in raw-specs
                 when (or (not value)
                          (car value))
                 collect
                 (if value
                     (make-instance spec-class
                                    :name name
                                    :function (parse-function (car value)))
                     (make-instance spec-class :name name)))))
    (loop for class in (c2mop:class-direct-superclasses class)
          when (typep class 'qt-class)
          do
          (loop for object in (slot-value class slot)
                ;; Search among raw-specs and not just rely on
                ;; pushnew, because raw-specs may override the
                ;; inclusion of a spec
                unless (find (name object) raw-specs
                             :key #'car :test #'equal)
                do (pushnew object result
                            :test #'equal
                            :key #'name)))
    (setf (slot-value class slot) result)))

(defun make-override-table (specs)
  (coerce specs 'vector))

(defun make-lisp-side-override-table (specs)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for spec in specs
          do (setf (gethash (name spec) ht)
                   (spec-function spec)))
    ht))

(defun compute-class-meta-data (class)
  (with-slots (qmetaobject qt-superclass slot-or-signal-table
               signals slots overrides
               override-table lisp-side-override-table)
      class
    (setf qmetaobject
          ;; clear out any old QMetaObject, so that ensure-qt-class-caches will
          ;; set up a new one
          nil)
    (compute-specs class 'signals 'signal-spec
                   (raw-signal-specs class))
    (compute-specs class 'slots 'slot-spec
                   (raw-slot-specs class))
    (compute-specs class 'overrides 'override-spec
                   (raw-override-specs class))
    (unless (eq (class-name class) 'dynamic-object)
      (setf qt-superclass
            (or qt-superclass
                (class-qt-superclass
                 (or (find-if (lambda (x) (typep x 'qt-class))
                              (c2mop:class-direct-superclasses class))
                     (error "No effective Qt class name declared for ~A"
                            class)))))
      (setf override-table
            (make-override-table overrides))
      (setf lisp-side-override-table
            (make-lisp-side-override-table overrides))
      (setf slot-or-signal-table (concatenate 'vector signals slots)))))

(defmethod c2mop:finalize-inheritance :after ((class qt-class))
  (dolist (super (c2mop:class-direct-superclasses class))
    (unless (c2mop:class-finalized-p super)
      (c2mop:finalize-inheritance super)))
  (compute-class-meta-data class)
  (loop for sub-class in (c2mop:class-direct-subclasses class)
        when (and (typep sub-class 'qt-class)
                  (c2mop:class-finalized-p sub-class))
        do
        (compute-class-meta-data class)))

(defun inform-cpp-about-override (qclass binding method-name
                                  override-id)
  (map-class-methods-named
   (lambda (<method>)
     (sw_override binding
                  (unbash* <method> +method+)
                  override-id))
   qclass
   method-name))

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

(defun set-class-binding (qt-class)
  (multiple-value-bind (idx <module>)
      (unbash* (slot-value qt-class 'effective-class) +class+)
    (declare (ignore idx))
    (setf (class-binding qt-class)
          (sw_make_dynamic_binding (module-ref <module>)
                                   (qobject-pointer
                                    (slot-value qt-class 'qmetaobject))
                                   (meta-object-method-index qt-class)
                                   (cffi:callback deletion-callback)
                                   (cffi:callback dynamic-invocation-callback)
                                   (cffi:callback child-callback)))))

(defun ensure-qt-class-caches (qt-class)
  (check-type qt-class qt-class)
  (with-slots (effective-class qmetaobject smoke-generation generation)
      qt-class
    (unless (and qmetaobject
		 effective-class
		 (eq smoke-generation *weakly-cached-objects*))
      ;; clear everything out to ensure a clean state in case of errors
      ;; in the following forms
      (setf effective-class nil)
      (setf qmetaobject nil)
      (setf smoke-generation nil)
      ;; reinitialize things
      (setf effective-class (find-qclass
                             (class-qt-superclass qt-class)))
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
                               (mapcar #'convert-slot-or-signal
                                       (class-signals qt-class))
                               (mapcar #'convert-slot-or-signal
                                       (class-slots qt-class)))))
      (set-class-binding qt-class)
      (inform-cpp-about-overrides qt-class)
      ;; invalidate call site caches
      (setf generation (gensym))
      ;; mark as fresh
      (setf (class-smoke-generation qt-class) *weakly-cached-objects*))))

(defun convert-slot-or-signal (member)
  (make-slot-or-signal (name member)))

(defun class-effective-class (qt-class &optional (errorp t))
  (ensure-qt-class-caches qt-class)
  (or (slot-value qt-class 'effective-class)
      (when errorp
        (error "effective-class not cached?"))))

(defun class-qmetaobject (qt-class)
  (ensure-qt-class-caches qt-class)
  (slot-value qt-class 'qmetaobject))

(defun find-method-override (object method)
  (if (typep object 'dynamic-object)
      (find-method-override-using-class (class-of object) method)
      nil))

(defun find-dynamic-method-override (object method-id)
  (if (typep object 'dynamic-object)
      (svref (override-table (class-of object))
             method-id)))

(defun find-method-override-using-class (class method)
  (gethash (qmethod-name method)
           (lisp-side-override-table class)))

(defvar *next-qmethod-trampoline* nil)
(defvar *next-qmethod* nil)

(defun call-next-qmethod (&rest args)
  (unless *next-qmethod-trampoline*
    (error "call-next-qmethod used outside of overriding method"))
  (funcall *next-qmethod-trampoline* args))

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

(defun qt_metacall-override (object call id stack)
  (let ((new-id (call-next-qmethod)))
    (cond
      ((or (minusp new-id)
           (not (eql (primitive-value call)
                     (primitive-value (#_QMetaObject::InvokeMetaMethod)))))
       id)
      (t
       (let ((member
              (or
               (get-slot-or-symbol (class-of object) new-id)
               (dynamic-slot-or-signal object new-id)
               (error "QT_METACALL-OVERRIDE: invalid member id ~A" id))))
         (etypecase member
           (signal-spec
            (#_activate (class-qmetaobject (class-of object))
                         object
                         id
                         stack)
            -1)
           (slot-spec
            (apply (spec-function member)
                   object
                   (unmarshal-slot-args member stack))
            -1)))))))

(defun guess-stack-item-slot (x)
  (case x
    (:|int| 'int)
    (:|uint| 'uint)
    (:|bool| 'bool)
    (:|QString| 'ptr)
    (t (error "Don't know how to unmarshal slot argument ~s" x))))

(defun ensure-slot-or-signal-types (member)
  (with-slots (cached-arg-types) member
    (unless (slot-boundp member 'cached-arg-types)
      (setf cached-arg-types
            (mapcar (lambda (name)
		      (or (find-qtype name)
			  (error "No smoke type found for dynamic member arg type ~A.  Giving up."
				 name)))
                    (cl-ppcre:split
                     ","
                     (entry-arg-types (convert-slot-or-signal member))))))
    cached-arg-types))

(defun unmarshal-slot-args (member argv)
  (iter (for type in (ensure-slot-or-signal-types member))
        (for i from 1)
        (collect (cond ((eq (qtype-interned-name type) ':|QString|)
                        (qstring-pointer-to-lisp
                         (cffi:mem-aref argv :pointer i)))
                       ((and
                         (eq (qtype-kind type) :stack)
                         (eq (qtype-stack-item-slot type) 'class))
                        (unmarshal type (cffi:inc-pointer argv
                                                          (* i
                                                             (cffi:foreign-type-size :pointer)))))
                       (t
                        (unmarshal type (cffi:mem-aref argv :pointer i)))))))

(defclass slot-or-signal ()
  ((name :initarg :name
         :accessor entry-name)
   (full-name :initarg :full-name
              :accessor entry-full-name)
   (arg-types :initarg :arg-types
              :accessor entry-arg-types)
   (reply-type :initarg :reply-type
               :accessor entry-reply-type)))

(defun make-slot-or-signal (str)
  (let ((str (#_data (#_QMetaObject::normalizedSignature str))))
    (or
     (cl-ppcre:register-groups-bind (a b c d)
         ("^(([\\w,<>:]*)\\s+)?([^\\s]*)\\((.*)\\)" str)
       (declare (ignore a))
       (make-instance 'slot-or-signal
                      :name c
                      :full-name (concatenate 'string c "(" d ")")
                      :arg-types d
                      :reply-type (if (or (null b) (equal b "void")) "" b)))
     (error "Invalid slot or signal signature: ~s" str))))

(defconstant +AccessPrivate+ #x00)
(defconstant +AccessProtected+ #x01)
(defconstant +AccessPublic+ #x02)
(defconstant +MethodMethod+ #x00)
(defconstant +MethodSignal+ #x04)
(defconstant +MethodSlot+ #x08)
(defconstant +MethodCompatibility+ #x10)
(defconstant +MethodCloned+ #x20)
(defconstant +MethodScriptable+ #x40)

(defun make-metaobject (parent class-name class-infos signals slots)
  (let ((data (make-array 0 :fill-pointer 0 :adjustable t))
        (table (make-hash-table))
        (stream (make-string-output-stream)))
    (labels ((intern-string (s)
               (or (gethash s table)
                   (setf (gethash s table)
                         (prog1
                             (file-position stream)
                           (write-string s stream)
                           (write-char (code-char 0) stream)))))
             (add (x) (vector-push-extend x data))
             (add-string (s) (add (intern-string s))))
      (add 1)                           ;revision
      (add (intern-string class-name))  ;class name
      (add (length class-infos))        ;classinfo
      (add (if (plusp (length class-infos)) 10 0))
      (add (+ (length signals) (length slots)))
      (add (+ 10 (* 2 (length class-infos)))) ;methods
      (add 0)                                 ;properties
      (add 0)
      (add 0)                           ;enums/sets
      (add 0)
      (dolist (entry class-infos)
        (add-string (entry-key entry))
        (add-string (entry-value entry)))
      (dolist (entry signals)
        (add-string (entry-full-name entry))
        (add-string (remove #\, (entry-full-name entry) :test-not #'eql))
        (add-string (entry-reply-type entry))
        (add-string "")                 ;tag
        (add (logior +methodsignal+ +accessprotected+)))
      (dolist (entry slots)
        (add-string (entry-full-name entry))
        (add-string (remove #\, (entry-full-name entry) :test-not #'eql))
        (add-string (entry-reply-type entry))
        (add-string "")                 ;tag
        (add (logior +methodslot+ +accesspublic+)))
      (add 0))
    (let ((dataptr (cffi:foreign-alloc :int :count (length data))))
      (dotimes (i (length data))
        (setf (cffi:mem-aref dataptr :int i) (elt data i)))
      (cache!
       (%qobject (find-qclass "QMetaObject")
                 (sw_make_metaobject (qobject-pointer parent)
                                     (cffi:foreign-string-alloc
                                      (get-output-stream-string stream))
                                     dataptr))))))

(defun call-with-signal-marshalling (fun types args)
  (let ((arg-count (length args)))
    (cffi:with-foreign-object (argv :pointer (1+ arg-count))
      (cffi:with-foreign-object (stack '|union StackItem| arg-count)
        (labels ((iterate (i rest-types rest-args)
                          (cond
                            (rest-args
                             (let* ((stack-item (cffi:mem-aref stack '|union StackItem| i))
                                    (arg (car rest-args))
                                    (type (car rest-types))
                                    (slot-type (qtype-stack-item-slot type)))
                               (marshal arg type stack-item
                                        (lambda ()
                                          (setf (cffi:mem-aref argv :pointer (1+ i))
                                                (if (or (eql slot-type 'ptr)
                                                        (eql slot-type 'class))
                                                    (cffi:mem-aref stack-item :pointer)
                                                    stack-item))
                                          (iterate (1+ i)
                                                   (cdr rest-types)
                                                   (cdr rest-args))))))
                            (t
                             (funcall fun argv)))))
          (iterate 0 types args))))))

(defun emit-signal (object name &rest args)
  (let* ((meta (class-qmetaobject (class-of object)))
         (signature (#_data (#_QMetaObject::normalizedSignature name)))
         (index (#_indexOfSignal meta signature))
         (types (and (>= index 0)
                     (mapcar (lambda (x) (find-qtype (#_data x)))
                             (#_parameterTypes (#_method meta index))))))
    (when (= index -1)
      (error "Signal ~a doesn't exist for ~s" signature object))
    (when (/= (length args) (length types))
      (error "Invalid number of arguments for signal ~a: ~a"
             signature (length args)))
    (call-with-signal-marshalling
     (lambda (stack)
       (list (#_QMetaObject::activate object index stack)))
     types
     args)))
