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

(defun pointer->cached-object (ptr)
  (gethash (cffi:pointer-address ptr) *cached-objects*))

(defun (setf pointer->cached-object) (newval ptr)
  (setf (gethash (cffi:pointer-address ptr) *cached-objects*)
        newval))

(defun %deletion-callback (obj)
  (restart-case
      (let ((object (pointer->cached-object obj)))
        (when object
          (note-deleted object)))
    (abort ()
      :report (lambda (stream) (write-string "Abort smoke callback" stream)))))

(defun %method-invocation-callback (smoke method-idx obj stack abstractp)
  (declare (ignore abstractp))
  (restart-case
      (let* ((<module> (module-number smoke))
             (object (pointer->cached-object obj))
             (<method> (bash method-idx <module> +method+))
             (fun (and object (find-method-override object <method>))))
        (if fun
            (let* ((args
                    (loop for type in (list-qmethod-argument-types <method>)
                       for i from 1
                       for item = (cffi:mem-aref stack
                                                 '|union StackItem|
                                                 i)
                       collect (unmarshal type item)))
                   (result (override fun object <method> args))
                   (rtype (qmethod-return-type <method>)))
              (unless (qtype-void-p rtype)
                (marshal result rtype stack (lambda ())))
              1)
            0))
    (abort ()
      :report (lambda (stream) (write-string "Abort smoke callback" stream))
      0)))

(defun %child-callback (added obj)
  (restart-case
      (let ((object (pointer->cached-object obj)))
        (when object
          (if (zerop added)
              (note-child-removed object)
              (note-child-added object))))
    (abort ()
      :report (lambda (stream) (write-string "Abort smoke callback" stream)))))

(defun map-qobject-children (fn x)
  (let ((*ptr-callback*
         (lambda (ptr)
           (funcall fn (%qobject (find-qclass "QObject") ptr)))))
    (sw_map_children (qobject-pointer x) (cffi:callback ptr-callback))))

(defun map-qobject-hierarchy (fn x)
  (funcall fn x)
  (map-qobject-children (lambda (y)
                          (map-qobject-hierarchy fn y))
                        x))

(defclass abstract-qobject ()
  ((class :initarg :class
          :accessor qobject-class)))

(defclass null-qobject (abstract-qobject)
  ())

(defun null-qobject (class)
  (make-instance 'null-qobject :class (find-qclass class)))

(defmethod qobject-pointer ((object null-qobject))
  (cffi:null-pointer))

(defclass qobject (abstract-qobject)
  ((pointer :initarg :pointer
            :initform :unborn
            :accessor qobject-pointer)
   (deleted :initform nil
            :accessor qobject-deleted)))

(defmethod print-object ((instance qobject) stream)
  (print-unreadable-object (instance stream :type nil :identity nil)
    (cond
      ((not (slot-boundp instance 'class))
       (format stream "uninitialized"))
      ((cffi:pointerp (qobject-pointer instance))
       (format stream "~A 0x~8,'0X"
               (qclass-name (qobject-class instance))
               (cffi:pointer-address (qobject-pointer instance))))
      (t
       (format stream "~A ~A"
               (qclass-name (qobject-class instance))
               (qobject-pointer instance))))))

(defmethod print-object ((instance null-qobject) stream)
  (print-unreadable-object (instance stream :type nil :identity nil)
    (format stream "~A NULL"
            (qclass-name (qobject-class instance)))))

(defclass primitive ()
  ((value :initarg :value :accessor primitive-value)))

(defmethod print-object ((instance primitive) stream)
  (print-unreadable-object (instance stream :type t :identity nil)
    (format stream "~A" (primitive-value instance))))

(defmacro defprimitive (name (superclass) type)
  `(progn
     (defclass ,name (,superclass) ())
     (defun ,name (value)
       (check-type value ,type)
       (make-instance ',name :value value))))

(defclass $ (primitive) ())
(defclass ? (primitive) ())

(defprimitive int ($) (signed-byte 32))
(defprimitive uint ($) (unsigned-byte 32))
(defprimitive bool ($) (signed-byte 32))

(defprimitive char* ($) (satisfies cffi:pointerp))
(defprimitive char** (?) (satisfies cffi:pointerp))
(defprimitive qstring ($) string)
(defprimitive qstringlist (?) (satisfies cffi:pointerp))
(defprimitive int& ($) (satisfies cffi:pointerp))
(defprimitive void** (?) (satisfies cffi:pointerp))
(defprimitive bool* ($) (satisfies cffi:pointerp))
(defprimitive quintptr (?) (satisfies cffi:pointerp))

(defclass enum ($)
  ((type-name :initarg :type-name
              :accessor enum-type-name)))

(defun enum (value type-name)
  (check-type value (signed-byte 32))
  (make-instance 'enum :type-name type-name :value value))

(defmethod print-object ((instance primitive) stream)
  (print-unreadable-object (instance stream :type t :identity nil)
    (format stream "~A"
            (primitive-value instance))))

(defmethod print-object ((instance enum) stream)
  (print-unreadable-object (instance stream :type t :identity nil)
    (format stream "~A ~A"
            (enum-type-name instance)
            (primitive-value instance))))

(defun enum= (a b)
  (and (eq (enum-type-name a) (enum-type-name b))
       (eql (primitive-value a) (primitive-value b))))

(defun qobject= (x y)
  (cffi-sys:pointer-eq (qobject-pointer x) (qobject-pointer y)))

(defun %qobject (class ptr)
  (or (pointer->cached-object ptr)
      (if (cffi:null-pointer-p ptr)
          (make-instance 'null-qobject :class class)
          (make-instance 'qobject :class class :pointer ptr))))

(defgeneric argument-munged-char (object))

(defmethod argument-munged-char ((object t))
  (error "don't know how to pass ~A to smoke functions" object))

(defmethod argument-munged-char ((object abstract-qobject)) #\#)
(defmethod argument-munged-char ((object $)) #\$)
(defmethod argument-munged-char ((object ?)) #\?)
(defmethod argument-munged-char ((object vector)) #\?)
(defmethod argument-munged-char ((object string)) #\$)
(defmethod argument-munged-char ((object integer)) #\$)
(defmethod argument-munged-char ((object real)) #\$)
(defmethod argument-munged-char ((object (eql t))) #\$)
(defmethod argument-munged-char ((object null)) #\$)
(defmethod argument-munged-char ((object qlist)) #\?)

(defmethod can-marshal-p ((kind t) (name t) (slot t) (arg t) (type t))
  nil)

(defmacro defmarshal ((kind name slot)
                      ((arg-var arg-type) type-var item-var &key test)
                      &body body)
  (let ((kind-var (gensym))
        (name-var (gensym))
        (slot-var (gensym))
        (cont-var (gensym)))
    `(progn
       (defmethod can-marshal-p ((kind ,kind)
                                 (name ,name)
                                 (slot ,slot)
                                 (arg ,arg-type)
                                 (type t))
         ,(if test
              `(funcall ,test arg type)
              t))
       (defmethod marshal-using-type ((,KIND-VAR ,kind)
                                      (,NAME-VAR ,name)
                                      (,SLOT-VAR ,slot)
                                      (,arg-var ,arg-type)
                                      ,type-var
                                      ,item-var
                                      ,CONT-VAR)
         ,@ (when test
              `((unless (funcall ,test ,arg-var ,type-var)
                  (error "argument ~A is not of the required type ~A"
                         ,arg-var ,type-var))))
         (macrolet ((marshal-next ()
                      `(funcall ,',CONT-VAR)))
           ,@body)))))

(defgeneric marshal-using-type (kind name slot arg type item cont))

(defgeneric find-applicable-method (object name args))

(defmethod find-applicable-method ((object abstract-qobject) method-name args)
  (qclass-find-applicable-method (qobject-class object) method-name args))

(defmethod find-applicable-method ((class integer) method-name args)
  (qclass-find-applicable-method class method-name args))

(defun type= (x y)
  (and (eq (qtype-kind x) (qtype-kind y))
       (eq (qtype-interned-name x) (qtype-interned-name y))
       (eq (qtype-stack-item-slot x) (qtype-stack-item-slot y))))

(defun method-signature= (a b)
  (let ((r (list-qmethod-argument-types a))
        (s (list-qmethod-argument-types b)))
    (and (eql (length r) (length s))
         (every #'type= r s))))

(defun arguments-to-munged-name (name args)
  (format nil "~A~{~C~}" name (mapcar #'argument-munged-char args)))

(defun qclass-find-applicable-method (class method-name args)
  (let ((munged-name (arguments-to-munged-name method-name args)))
    (labels ((recurse (c)
               (append (list-methodmap-methods (find-methodmap c munged-name))
                       (some #'recurse (list-qclass-superclasses c)))))
      (let ((methods (remove-duplicates (recurse class)
                                        :from-end t
                                        :test #'method-signature=)))
        (cond
          ((null methods)
           nil)
          ((cdr methods)
           (find-if (lambda (method)
                      (method-applicable-p method args))
                    methods))
          (t
           (car methods)))))))

(defun method-applicable-p (method args)
  (every (lambda (type arg)
           (can-marshal-p (qtype-kind type)
                          (qtype-interned-name type)
                          (qtype-stack-item-slot type)
                          arg
                          type))
         (list-qmethod-argument-types method)
         args))

(defun marshal (argument type stack-item cont)
  (marshal-using-type (qtype-kind type)
                      (qtype-interned-name type)
                      (qtype-stack-item-slot type)
                      argument
                      type
                      stack-item
                      cont))

(defun qtypep (instance thing)
  (let ((kind (nth-value 2 (unbash thing))))
    (cond
     ((eql kind +class+) (qsubclassp (qobject-class instance) thing))
     ((eql kind +type+) (qtypep instance (qtype-class thing)))
     (t (error "not a type or class: ~A" thing)))))

(defun qsubclassp (a b)
  (or (eq a b)
      (some (lambda (super) (qsubclassp super b))
            (list-qclass-superclasses a))))

;; for reference results, return new values as multiple return values
(defun splice-reference-result (result-list newval)
  (destructuring-bind (primary-return-value &rest rest) result-list
    (list* primary-return-value newval rest)))

(defun string-vector-to-char**! (ptr vector)
  (loop
     for i from 0
     for elt across vector
     do
       (setf (cffi:mem-aref ptr :pointer i)
             (cffi:foreign-string-alloc elt))))

(defun string-vector-to-char** (vector)
  (let ((ptr (cffi:foreign-alloc :pointer :count (length vector))))
    (string-vector-to-char**! ptr vector)
    ptr))

(defun char**-to-string-vector! (vector ptr n freep)
  (loop
     for i from 0 below n
     do
       (setf (elt vector i)
             (cffi:mem-aref ptr :string i))
       (when freep
         (cffi:foreign-free (cffi:mem-aref ptr :pointer i)))))

(defun char**-to-string-vector (ptr n freep)
  (let ((vector (make-array n)))
    (char**-to-string-vector! vector ptr n freep)
    vector))

(defgeneric unmarshal-using-type (kind name item type stack))

(defun unmarshal (type stack-item)
  (unmarshal-using-type (qtype-kind type)
                        (qtype-interned-name type)
                        (qtype-stack-item-slot type)
                        type
                        stack-item))

(defun call-with-marshalling (fun types args)
  (cffi:with-foreign-object (stack '|union StackItem| (1+ (length args)))
    (labels ((iterate (i rest-types rest-args)
               (if rest-args
                   (marshal (car rest-args)
                            (car rest-types)
                            (cffi:mem-aref stack '|union StackItem| i)
                            (lambda ()
                              (iterate (1+ i)
                                       (cdr rest-types)
                                       (cdr rest-args))))
                   (funcall fun stack))))
      (iterate 1 types args))))

(defmethod new ((qclass string) &rest args)
  (apply #'new (find-qclass qclass) args))

(defun qpointer-target-already-deleted-p (qp)
  (logbitp 0 (sw_qpointer_is_null qp)))

(defun null-qobject-p (object)
  (typep object 'null-qobject))

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

#+(or)
(defun run-pending ()
  (setf *pending-finalizations*
        (remove-if #'funcall *pending-finalizations*)))

(defun cache! (object)
  (assert (null (pointer->cached-object (qobject-pointer object))))
  (setf (pointer->cached-object (qobject-pointer object)) object)
  (when (or (not (qtypep object (find-qclass "QObject")))
	    (typep (#_parent object) 'null-qobject))
    (tg:finalize object
		 (let* ((ptr (qobject-pointer object))
			(class (qobject-class object))
			(str (princ-to-string object))
			(qobjectp (qsubclassp class (find-qclass "QObject")))
			(dynamicp (typep object 'dynamic-object)))
		   (lambda ()
		     (postmortem ptr class str qobjectp dynamicp)))))
  object)

(defmethod new ((class integer) &rest args)
  (apply #'new
         (make-instance 'qobject
                        :class class
                        :pointer :unborn)
         args))

(defun %call-ctor (method stack binding)
  (cffi:foreign-funcall-pointer
   (qclass-trampoline-fun (qmethod-class method))
   ()
   :short (qmethod-arg-for-classfn method)
   :pointer (cffi:null-pointer)
   :pointer stack
   :void)
  (let ((new-object (cffi:foreign-slot-value stack '|union StackItem| 'ptr)))
    (cffi:with-foreign-object (stack2 '|union StackItem| 2)
      (setf (cffi:foreign-slot-value
             (cffi:mem-aref stack2 '|union StackItem| 1)
             '|union StackItem|
             'ptr)
            binding)
      (cffi:foreign-funcall-pointer
       (qclass-trampoline-fun (qmethod-class method))
       ()
       :short 0
       :pointer new-object
       :pointer stack2
       :void))
    new-object))

(defun binding-for-ctor (method instance)
  (let* ((<module> (ldb-module (qmethod-class method)))
	 (data (data-ref <module>)))
    (if (typep instance 'dynamic-object)
	(data-fat data)
	(data-thin data))))

(defmethod new ((instance qobject) &rest args)
  (let* ((class (qobject-class instance))
         (method (qclass-find-applicable-method class (qclass-name class) args)))
    (unless method
      (error "No applicable constructor ~A found for arguments ~A"
             (qclass-name class) args))
    (assert (eq class (qtype-class (qmethod-return-type method))))
    (apply #'values
	   (call-with-marshalling
	    (lambda (stack)
	      (setf (qobject-pointer instance)
		    (%call-ctor method
				stack
				(binding-for-ctor method instance)))
	      (cache! instance)
	      (list instance))
	    (list-qmethod-argument-types method)
	    args))))

(defun call (instance method &rest args)
  (%call t instance method args))

(defun call-without-override (instance method &rest args)
  (%call nil instance method args))

(defun %call (allow-override-p instance method args)
  (typecase instance
    (symbol
     (setf instance (class-effective-class (find-class instance))))
    (qt-class
     (setf instance (class-effective-class instance)))
    (string
     (setf instance (find-qclass instance))))
  (let ((name method)
        (method (etypecase method
		  (integer method)
		  (string (find-applicable-method instance method args)))))
    (unless method
      (error "No applicable method ~A found on ~A with arguments ~A"
             name instance args))
    (when (typep instance 'integer)
      (unless (qmethod-static-p method)
        (error "not a static method"))
      (setf instance (null-qobject instance)))
    (let ((rtype (qmethod-return-type method)))
      (apply #'values
	     (call-with-marshalling
	      (lambda (stack &aux fun)
		(cond
		 ((and allow-override-p
		       (setf fun
			     (find-method-override instance method)))
		  (multiple-value-list (override fun instance method args)))
		 (t
		  (cffi:foreign-funcall-pointer
		   (qclass-trampoline-fun (qmethod-class method))
		   ()
		   :short (qmethod-arg-for-classfn method)
		   :pointer (if (null-qobject-p instance)
                                (cffi:null-pointer)
                                (%cast instance (qmethod-class method)))
		   :pointer stack
		   :void)
		  (list (and (not (qtype-void-p rtype))
			     (unmarshal rtype stack))))))
	      (list-qmethod-argument-types method)
	      args)))))

(defun note-deleted (object)
  (check-type object abstract-qobject)
  (unless (qobject-deleted object)
    (tg:cancel-finalization object)
    (remhash (cffi:pointer-address (qobject-pointer object)) *cached-objects*)
    (setf (qobject-deleted object) t)))

(defun delete-object (object)
  (cond
    ((typep object 'null-qobject)
     (error "cannot delete null object: ~A" object))
    ((qobject-deleted object)
     (warn "not deleting dead object: ~A" object))
    (t
     #+nil (sw_delete (qobject-pointer object))
     (call object (format nil "~~~A" (qclass-name (qobject-class object))))
     (note-deleted object))))

(defmacro with-object ((var &optional value) &body body)
  (if value
      `(call-with-object (lambda (,var) ,@body) ,value)
      `(let ((,var nil))
         (flet ((,var (x) (push x ,var) x))
           (unwind-protect
                (progn ,@body)
             (mapc #'maybe-delete-object ,var))))))

(defmacro with-objects ((&rest clauses) &body body)
  (if clauses
      `(with-object ,(car clauses) (with-objects ,(rest clauses) ,@body))
      `(progn ,@body)))

(defun maybe-delete-object (object)
  (unless (or (typep object 'null-qobject)
              (qobject-deleted object))
    (delete-object object)))

(defun call-with-object (fun object)
  (check-type object abstract-qobject)
  (unwind-protect
       (funcall fun object)
    (maybe-delete-object object)))

(defmethod note-child-added ((object qobject))
  (setf (gethash object *keep-alive*) t))

(defmethod note-child-removed ((object qobject))
  (remhash object *keep-alive*))

(defun map-cpl (fun class)
  (labels ((recurse (c)
             (funcall fun c)
             (map-qclass-superclasses #'recurse c)))
    (recurse class)))
