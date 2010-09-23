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
  (gethash (cffi:pointer-address ptr) *weakly-cached-objects*))

(defun (setf pointer->cached-object) (newval ptr)
  (setf (gethash (cffi:pointer-address ptr) *weakly-cached-objects*)
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

;;; (defprimitive int ($) (signed-byte 32))
;;; (defprimitive uint ($) (unsigned-byte 32))
;;; (defprimitive bool ($) (signed-byte 32))

;;; (defprimitive char* ($) (satisfies cffi:pointerp))
;;; (defprimitive char** (?) (satisfies cffi:pointerp))
;;; (defprimitive qstring ($) string)
;;; (defprimitive qstringlist (?) (satisfies cffi:pointerp))
;;; (defprimitive int& ($) (satisfies cffi:pointerp))
;;; (defprimitive void** (?) (satisfies cffi:pointerp))
;;; (defprimitive bool* ($) (satisfies cffi:pointerp))
;;; (defprimitive quintptr (?) (satisfies cffi:pointerp))

(defclass enum ($)
  ((type-name :initarg :type-name
              :accessor enum-type-name)))

(defun enum (value type-name)
  (check-type value (signed-byte 32))
  (make-instance 'enum :type-name type-name :value value))

#+nil
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

(defun enum-or (&rest enums)
  (reduce #'logior enums
          :key (lambda (x) (if (integerp x) x (primitive-value x)))))

(defclass qthread ()
  ((pointer :initarg :pointer
            :accessor qthread-pointer)))

(defmethod print-object ((instance qthread) stream)
  (print-unreadable-object (instance stream :type t :identity nil)
    (format stream "~X" (cffi:pointer-address (qthread-pointer instance)))))

(defun qobject= (x y)
  (cffi-sys:pointer-eq (qobject-pointer x) (qobject-pointer y)))

(defun %qobject (class ptr)
  (let ((cached (pointer->cached-object ptr)))
    (if (and cached
             (= (qobject-class cached) class))
        cached
        (if (cffi:null-pointer-p ptr)
            (make-instance 'null-qobject :class class)
            (make-instance 'qobject :class class :pointer ptr)))))

(flet ((note-lisp-type-for-stack-slot (slot type)
         (setf (get slot 'lisp-type-for-stack-slot) type)))
  #+nil (note-lisp-type-for-stack-slot 'class 'abstract-qobject)
  (note-lisp-type-for-stack-slot 'ptr '(or (satisfies cffi-sys:pointerp)
                                        string))
  (note-lisp-type-for-stack-slot 'bool 't #+nil boolean)
  (note-lisp-type-for-stack-slot 'enum '(or (unsigned-byte 32) enum))
  (note-lisp-type-for-stack-slot 'uint '(or (unsigned-byte 32) enum))
  (note-lisp-type-for-stack-slot 'int '(or (signed-byte 32) enum))
  (note-lisp-type-for-stack-slot 'ulong '(unsigned-byte 64)) ;fixme
  (note-lisp-type-for-stack-slot 'long '(signed-byte 64))    ;fixme
  (note-lisp-type-for-stack-slot 'ushort '(unsigned-byte 16))
  (note-lisp-type-for-stack-slot 'short '(signed-byte 16))
  (note-lisp-type-for-stack-slot 'float 'number)
  (note-lisp-type-for-stack-slot 'double 'number))

#+nil
(flet ((note-lisp-type-for-qtype (interned-type-name type)
         (setf (get interned-type-name 'lisp-type-for-qtype) type)))
  (note-lisp-type-for-qtype :|const QString&| 'string)
  (note-lisp-type-for-qtype :|const char*| 'string)
  (note-lisp-type-for-qtype :|const QList<int>&| 'qlist<int>))

(defun can-marshal-p (lisp-object <type>)
  (let ((slot (qtype-stack-item-slot <type>)))
    (if (eq slot 'class)
        (and (typep  lisp-object 'abstract-qobject)
             (qtypep lisp-object (qtype-class <type>)))
        (typep lisp-object
               `(and ,(or (get slot 'lisp-type-for-stack-slot)
                          (progn
                            (warn "slot ~A not implemented"
                                  (qtype-stack-item-slot <type>))
                            nil))
                     #+nil
                     ,(get (qtype-interned-name <type>)
                           'lisp-type-for-qtype t))))))

(defun find-applicable-method (object name args fix-types)
  (qclass-find-applicable-method (if (integerp object)
                                     object
                                     (qobject-class object))
                                 name
                                 args
                                 fix-types))

(defun type= (x y)
  (and (eq (qtype-kind x) (qtype-kind y))
       (eq (qtype-interned-name x) (qtype-interned-name y))
       (eq (qtype-stack-item-slot x) (qtype-stack-item-slot y))))

(defun method-signature= (a b)
  (let ((r (list-qmethod-argument-types a))
        (s (list-qmethod-argument-types b)))
    (and (eql (length r) (length s))
         (every #'type= r s))))

(defun qclass-find-applicable-method (class method-name args fix-types)
  (labels ((recurse (c)
             (append (list-class-methods-named c method-name)
                     (iter (for super in (list-qclass-superclasses c))
                           (appending (recurse super))))))
    (let ((methods #+nil (remove-duplicates (recurse class)
                                      :from-end t
                                      :test #'method-signature=)
                   (recurse class)))
      (cond
        ((null methods)
         nil)
        ((cdr methods)
         (find-if (lambda (method)
                    (method-applicable-p method args fix-types))
                  methods))
        (t
         (car methods))))))

(defun method-applicable-p (method args &optional fix-types)
  (let ((argtypes (list-qmethod-argument-types method)))
    (and (iter (for method-argtype in argtypes)
               (for fix-type in fix-types)
               (always (or (null fix-type)
                           (eq (qtype-interned-name method-argtype)
                               fix-type))))
         (eql (length argtypes) (length args))
         (every #'can-marshal-p args argtypes))))

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

(defun unmarshal (type stack-item)
  (unmarshal-using-type type stack-item))

(defun null-qobject-p (object)
  (typep object 'null-qobject))


#+(or)
(defun run-pending ()
  (setf *pending-finalizations*
        (remove-if #'funcall *pending-finalizations*)))

(defvar *report-memory-leaks* nil)

(defun binding-for-ctor (method instance)
  (let* ((<module> (ldb-module (qmethod-class method)))
	 (data (data-ref <module>)))
    (if (typep instance 'dynamic-object)
	(data-fat data)
	(data-thin data))))

;; old-style NEW usage for INITIALIZE-INSTANCE methods kept around for
;; compatibility.
(defun new (instance &rest args)
  (check-type instance dynamic-object)
  (apply #'interpret-new instance args))

(defun interpret-new (class-or-instance &rest args)
  (let ((instance (full-resolve-ctor-this class-or-instance)))
    (funcall (resolve-new instance args) instance args)))

(defmacro optimized-new (class-or-instance &rest args)
  (multiple-value-bind (fix-types args)
      (parse-optimized-call-args args)
    (let ((argsyms (iter (for i from 0 below (length args))
                         (collect (make-symbol (format nil "ARG~D" i)))))
          (sigsyms (iter (for i from 0 below (length args))
                         (collect (make-symbol (format nil "SIG~D" i))))))
      `(let ((instance ,(compile-time-resolve-ctor-this class-or-instance))
             (types ',fix-types)
             ,@(iter (for arg in args)
                     (for sym in argsyms)
                     (collect `(,sym ,arg))))
         (let ((args (list ,@argsyms))
               ,@(iter (for sig in sigsyms)
                       (for arg in argsyms)
                       (collect `(,sig (signature-type ,arg)))))
           (multiple-value-bind (instance-qclass instance-extra-sig)
               (typecase instance
                 (integer
                  (values instance :static))
                 (dynamic-object
                  (values (qobject-class instance)
                          (class-generation (class-of instance))))
                 (t
                  (values (qobject-class instance) :instance)))
             (cached-values-bind (fun)
                 (resolve-new instance args types)
               (provided instance-qclass :hash t)
               (provided instance-extra-sig)
               ,@(iter (for sig in sigsyms)
                       (collect `(provided ,sig :hash sxhash)))
               (funcall fun instance args))))))))

(defun resolve-new (instance args &optional fix-types)
  ;; (format *trace-output* "cache miss for #_new ~A~%" instance)
  (let* ((class (qobject-class instance))
         (method
          (qclass-find-applicable-method class
                                         (qclass-name class)
                                         args
                                         fix-types)))
    (unless method
      (error "No applicable constructor ~A found for arguments ~A"
             (qclass-name class) args))
    (assert (eq class (qtype-class (qmethod-return-type method))))
    (let ((trampfun (qclass-trampoline-fun (qmethod-class method)))
          (arg-for-trampfun (qmethod-arg-for-classfn method))
          (binding (binding-for-ctor method instance))
          (arglist-marshaller
           (arglist-marshaller args (list-qmethod-argument-types method))))
      (lambda (instance args)
        (%%new instance
               args
               arglist-marshaller
               trampfun
               arg-for-trampfun
               binding)))))

(defun %%new (instance
              args
              arglist-marshaller
              trampfun
              arg-for-trampfun
              binding)
  (funcall arglist-marshaller
           args
           (lambda (stack)
             (cffi:foreign-funcall-pointer
              trampfun
              ()
              :short arg-for-trampfun
              :pointer (cffi:null-pointer)
              :pointer stack
              :void)
             (let ((new-object
                    (cffi:foreign-slot-value stack '|union StackItem| 'ptr)))
               (cffi:with-foreign-object (stack2 '|union StackItem| 2)
                 (setf (cffi:foreign-slot-value
                        (cffi:mem-aref stack2 '|union StackItem| 1)
                        '|union StackItem|
                        'ptr)
                       binding)
                 (cffi:foreign-funcall-pointer
                  trampfun
                  ()
                  :short 0
                  :pointer new-object
                  :pointer stack2
                  :void))
               (setf (qobject-pointer instance) new-object))
             (cache! instance)
             instance)))

(defun interpret-call (instance method &rest args)
  (%interpret-call t instance method args))

(defun interpret-call-without-override (instance method &rest args)
  (%interpret-call nil instance method args))

(defun full-resolve-this (instance)
  (etypecase instance
    (qobject  instance)
    (integer  instance)
    (symbol   (class-effective-class (find-class instance)))
    (qt-class (class-effective-class instance))
    (string   (find-qclass instance))))

(defun compile-time-resolve-this (instance)
  (etypecase instance
    (string `(with-cache () (find-qclass ,instance)))
    ((cons (eql quote) (cons symbol null))
     `(class-effective-class (find-class ,instance)))
    (t `(full-resolve-this ,instance))))

(defun full-resolve-ctor-this (instance)
  (typecase instance
    (qobject
     instance)
    (integer
     (make-instance 'qobject :class instance :pointer :unborn))
    (string
     (make-instance 'qobject :class (find-qclass instance) :pointer :unborn))
    (qt-class
     (make-instance instance :pointer :unborn))))

(defun compile-time-resolve-ctor-this (instance)
  (etypecase instance
    (string `(make-instance 'qobject
                            :class (with-cache () (find-qclass ,instance))
                            :pointer :unborn))
    (t `(full-resolve-ctor-this ,instance))))

(declaim (inline %%call))
(defun %%call (casted-instance-pointer
               args
               arglist-marshaller
               trampfun
               arg-for-trampfun
               return-value-function)
  (funcall arglist-marshaller
           args
           (lambda (stack)
             (cffi:foreign-funcall-pointer
              trampfun
              ()
              :short arg-for-trampfun
              :pointer casted-instance-pointer
              :pointer stack
              :void)
             (funcall return-value-function stack))))

(declaim (inline %%call/override))
(defun %%call/override (precompiled-override instance method args)
  (override precompiled-override instance method args))

(defun argstep-marshaller (for-values argtypes i)
  (if argtypes
      (let ((marshal-thunk (marshaller (car for-values)
                                       (car argtypes)))
            (next-thunk (argstep-marshaller (cdr for-values)
                                            (cdr argtypes)
                                            (1+ i))))
        (lambda (stack arglist final-cont)
          (funcall marshal-thunk
                   (car arglist)
                   (cffi:mem-aref stack '|union StackItem| i)
                   (lambda ()
                     (funcall next-thunk
                              stack
                              (cdr arglist)
                              final-cont)))))
      (lambda (stack arglist final-cont)
        (declare (ignore arglist))
        (funcall final-cont stack))))

(defun arglist-marshaller (for-values argtypes)
  (let ((thunk (argstep-marshaller for-values argtypes 1))
        (n (1+ (length argtypes))))
    (lambda (arglist final-cont)
      (cffi:with-foreign-object (stack '|union StackItem| n)
        (funcall thunk stack arglist final-cont)))))

(defun resolve-call (allow-override-p instance method args &optional fix-types)
  ;; (format *trace-output* "cache miss for ~A::~A~%" instance method)
  (let ((name method)
        (method (etypecase method
		  (integer method)
		  (string (find-applicable-method
                           instance method args fix-types)))))
    (unless method
      (error "No applicable method ~A found on ~A with arguments ~A"
             name instance args))
    (let* ((precompiled-override
            (when (and allow-override-p (typep instance 'dynamic-object))
              (find-method-override instance method)))
           (arglist-marshaller
            (arglist-marshaller args (list-qmethod-argument-types method)))
           (trampfun
            (qclass-trampoline-fun (qmethod-class method)))
           (arg-for-trampfun
            (qmethod-arg-for-classfn method))
           (rtype
            (qmethod-return-type method))
           (return-value-function
            (unmarshaller rtype)))
      (cond
        ((integerp instance)
         (unless (qmethod-static-p method)
           (error "not a static method"))
         (assert (not precompiled-override))
         (lambda (<class> args)
           (declare (ignore <class>))
           (%%call (CFFI:NULL-POINTER)
                   args
                   arglist-marshaller
                   trampfun
                   arg-for-trampfun
                   return-value-function)))
        (t
         (let ((<from> (qobject-class instance)))
           (multiple-value-bind (castfn <to>)
               (resolve-cast <from> (qmethod-class method))
             (cond
               ((alexandria:starts-with #\~ (qmethod-name method))
                (if precompiled-override
                    (lambda (actual-instance args)
                      (NOTE-DELETED ACTUAL-INSTANCE)
                      (%%call/override precompiled-override
                                       ACTUAL-INSTANCE
                                       method
                                       args))
                    (lambda (actual-instance args)
                      (NOTE-DELETED ACTUAL-INSTANCE)
                      (%%call (PERFORM-CAST ACTUAL-INSTANCE CASTFN <FROM> <TO>)
                              args
                              arglist-marshaller
                              trampfun
                              arg-for-trampfun
                              return-value-function))))
               (t
                (if precompiled-override
                    (lambda (actual-instance args)
                      (%%call/override precompiled-override
                                       ACTUAL-INSTANCE
                                       method
                                       args))
                    (lambda (actual-instance args)
                      (%%call (PERFORM-CAST ACTUAL-INSTANCE CASTFN <FROM> <TO>)
                              args
                              arglist-marshaller
                              trampfun
                              arg-for-trampfun
                              return-value-function))))))))))))

(defun %interpret-call (allow-override-p instance method args)
  (let ((instance (full-resolve-this instance)))
    (funcall (resolve-call allow-override-p instance method args)
             instance
             args)))

;;(declaim (inline signature-type))
(defun signature-type (object)
  (typecase object
    ((and abstract-qobject (not dynamic-object))
     (qobject-class object))
    (string
     ;; avoid having the length slip into the type
     'string)
    ;; similar issues:
    ((signed-byte 32) '(signed-byte 32))
    ((unsigned-byte 32) '(unsigned-byte 32))
    ((signed-byte 64) '(signed-byte 64))
    ((unsigned-byte 64) '(unsigned-byte 64))
    (t
     (type-of object))))

(defun parse-optimized-call-args (forms)
  (let ((type nil))
    (iter (for form in forms)
          (cond
            ((keywordp form)
             (when type
               (error "duplicate type specification: ~A / ~A" type form))
             (setf type form))
            (t
             (collect type into types)
             (collect form into clean-forms)
             (setf type nil)))
          (finally
           (return (values types clean-forms))))))

(defmacro optimized-call (allow-override-p instance method &rest args)
  (multiple-value-bind (fix-types args)
      (parse-optimized-call-args args)
    (let ((argsyms (iter (for i from 0 below (length args))
                         (collect (make-symbol (format nil "ARG~D" i)))))
          (sigsyms (iter (for i from 0 below (length args))
                         (collect (make-symbol (format nil "SIG~D" i))))))
      `(let ((allow-override-p ,allow-override-p)
             (instance ,(compile-time-resolve-this instance))
             (method ,method)
             (types ',fix-types)
             ,@(iter (for arg in args)
                     (for sym in argsyms)
                     (collect `(,sym ,arg))))
         (let ((args (list ,@argsyms))
               ,@(iter (for sig in sigsyms)
                       (for arg in argsyms)
                       (collect `(,sig (signature-type ,arg)))))
           (multiple-value-bind (instance-qclass instance-extra-sig)
               (typecase instance
                 (integer
                  (values instance :static))
                 (dynamic-object
                  (values (qobject-class instance)
                          (class-generation (class-of instance))))
                 (t
                  (values (qobject-class instance) :instance)))
             (cached-values-bind (fun)
                 (resolve-call allow-override-p instance method args types)
               (provided instance-qclass :hash t)
               (provided instance-extra-sig)
               (provided method)
               ,@(iter (for sig in sigsyms)
                       (collect `(provided ,sig :hash sxhash)))
               (funcall fun instance args))))))))

(defun note-deleted (object)
  (check-type object abstract-qobject)
  (unless (qobject-deleted object)
    (cancel-finalization object)
    (let ((addr (cffi:pointer-address (qobject-pointer object))))
      (remhash addr *weakly-cached-objects*)
      (remhash addr *strongly-cached-objects*))
    (setf (qobject-deleted object) t)))

(defun cancel-finalization (object)
  (check-type object abstract-qobject)
  (tg:cancel-finalization object))

;; like the OPTIMIZED macros, this cannot be a function because that would
;; foil caching
(defmacro %maybe-delete (object)
  `(let ((object ,object))
     (unless (or (typep object 'null-qobject)
                 (qobject-deleted object))
       (optimized-delete object))))

(defmacro with-object ((var &optional value) &body body)
  ;; must not be implemented using a call-with-object function
  (if value
      `(let ((,var ,value))
         (unwind-protect
              (progn ,@body)
           (%maybe-delete ,var)))
      `(let ((,var nil))
         (flet ((,var (x) (push x ,var) x))
           (unwind-protect
                (progn ,@body)
             (dolist (object ,var)
               (%maybe-delete object)))))))

(defmacro with-objects ((&rest clauses) &body body)
  (if clauses
      `(with-object ,(car clauses) (with-objects ,(rest clauses) ,@body))
      `(progn ,@body)))

(defmethod note-child-added ((object qobject))
  (setf (gethash object *keep-alive*) t))

(defmethod note-child-removed ((object qobject))
  (remhash object *keep-alive*))

(defun map-cpl (fun class)
  (labels ((recurse (c)
             (funcall fun c)
             (map-qclass-superclasses #'recurse c)))
    (recurse class)))
