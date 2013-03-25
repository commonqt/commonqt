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

(defun binding-for-ctor (method instance)
  (if (typep instance 'dynamic-object)
      (class-binding (class-of instance))
      (data-binding (data-ref (ldb-module (qmethod-class method))))))

;; old-style NEW usage for INITIALIZE-INSTANCE methods kept around for
;; compatibility.
(defmacro new (instance &rest args)
  `(optimized-new ,instance ,@args))

(defun interpret-new (class-or-instance &rest args)
  (let ((instance (full-resolve-ctor-this class-or-instance)))
    (funcall (resolve-new instance args) instance args)))

(defun make-symbols (prefix number)
  (loop for i below number
        collect (make-symbol (format nil "~A-~D" prefix i))))

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
               (error "Duplicate type specification: ~A / ~A" type form))
             (setf type form))
            (t
             (collect type into types)
             (collect form into clean-forms)
             (setf type nil)))
          (finally
           (return (values types clean-forms))))))

(defun make-optimized (instance method
                       &key instance-resolver args resolver
                            env)
  (flet ((number-of-non-constantp (list)
           (loop for x in list
                 count (not (constantp x env)))))
   (multiple-value-bind (fix-types args) (parse-optimized-call-args args)
     (let ((argsyms (make-symbols 'arg (length args)))
           (sigsyms (make-symbols 'sig (number-of-non-constantp args))))
       ;; FIXME: check evaluation order
       `(let* (,@(iter (for arg in args)
                   (for sym in argsyms)
                   (unless (constantp arg env)
                     (collect `(,sym ,arg))))
               (instance ,(funcall instance-resolver instance))
               (method ,method)
               (types ',fix-types)
               (args ,(if (zerop (number-of-non-constantp args))
                          `',args
                          `(list*
                            ,@(loop for (arg . rest) on args
                                    for argsym in argsyms
                                    collect
                                    (if (constantp arg env)
                                        arg
                                        argsym)
                                    if (zerop (number-of-non-constantp rest))
                                    collect `',rest
                                    and
                                    do (loop-finish)))))
               ,@(loop with sigs = sigsyms
                       for arg in args
                       for argsym in argsyms
                       unless (constantp arg env)
                       collect `(,(pop sigs) (signature-type ,argsym))))
          (declare (dynamic-extent args))
          (multiple-value-bind (instance-qclass instance-extra-sig)
              (typecase instance
                (integer
                 (values instance :static))
                (dynamic-object
                 (values (qobject-class instance)
                         (class-generation (class-of instance))))
                (t
                 (values (qobject-class instance) :instance)))
            (cached-values-bind (fun) ,resolver
                ((instance-qclass :hash t)
                 (instance-extra-sig)
                 (method)
                 ,@(loop for sig in sigsyms
                         collect `(,sig :hash sxhash)))
              (funcall fun instance args))))))))

(defmacro optimized-call (allow-override-p instance method &rest args
                          &environment env)
  (make-optimized instance method
    :instance-resolver #'compile-time-resolve-this
    :args args
    :resolver `(resolve-call ,allow-override-p instance method args types)
    :env env))

(defmacro optimized-new (class-or-instance &rest args
                         &environment env)
  (make-optimized class-or-instance nil
    :instance-resolver #'compile-time-resolve-ctor-this
    :args args
    :resolver `(resolve-new instance args types)
    :env env))

(defun resolve-call (allow-override-p instance method args &optional fix-types)
  ;; (format *trace-output* "cache miss for ~A::~A~%" instance method)
  (let ((name method)
        (method (etypecase method
                  (integer method)
                  (string (find-applicable-method
                           instance method args fix-types)))))
    (unless method
      (error "No applicable method ~A found on ~A with arguments ~S"
             name instance args))
    (let* ((precompiled-override
             (when allow-override-p
               (find-method-override instance method)))
           (arglist-marshaller
             (arglist-marshaller args (list-qmethod-argument-types method)))
           (classfn
             (qclass-trampoline-fun (qmethod-class method)))
           (method-index
             (qmethod-classfn-index method))
           (rtype
             (qmethod-return-type method))
           (return-value-function
             (unmarshaller rtype)))
      (cond
        ((integerp instance)
         (unless (qmethod-static-p method)
           (error "~a::~a is not a static method"
                  (qclass-name instance) name))
         (assert (not precompiled-override))
         (lambda (<class> args)
           (declare (ignore <class>))
           (%%call (cffi:null-pointer)
                   args
                   arglist-marshaller
                   classfn
                   method-index
                   return-value-function)))
        (t
         (let ((<from> (qobject-class instance)))
           (multiple-value-bind (castfn <to>)
               (resolve-cast <from> (qmethod-class method))
             (let ((cont
                     (if precompiled-override
                         (lambda (actual-instance args)
                           (override precompiled-override
                                     actual-instance method args))
                         (lambda (actual-instance args)
                           (%%call (perform-cast actual-instance castfn <from> <to>)
                                   args
                                   arglist-marshaller
                                   classfn
                                   method-index
                                   return-value-function)))))
               (if (alexandria:starts-with #\~ (qmethod-name method))
                   (lambda (actual-instance args)
                     (prog1 (funcall cont actual-instance args)
                       (note-deleted actual-instance)))
                   cont)))))))))

(defun resolve-new (instance args &optional fix-types)
  ;; (format *trace-output* "cache miss for #_new ~A~%" instance)
  (let* ((class (qobject-class instance))
         (method
           (qclass-find-applicable-method class
                                          (qclass-name class)
                                          args
                                          fix-types)))
    (unless method
      (error "No applicable constructor ~A found for arguments ~S"
             (qclass-name class) args))
    (assert (eql class (qtype-class (qmethod-return-type method))))
    (let ((classfn (qclass-trampoline-fun (qmethod-class method)))
          (method-index (qmethod-classfn-index method))
          (binding (binding-for-ctor method instance))
          (arglist-marshaller
            (arglist-marshaller args (list-qmethod-argument-types method))))
      (named-lambda new-continuation (instance args)
        (%%new instance
               args
               arglist-marshaller
               classfn
               method-index
               binding)))))

(defun set-object-binding (classfn object binding)
  (cffi:with-foreign-object (stack '(:union StackItem) 2)
    (setf (cffi:foreign-slot-value
           (cffi:mem-aptr stack '(:union StackItem) 1)
           '(:union StackItem)
           'ptr)
          binding)
    ;; Method index 0 sets the binding
    (call-class-fun classfn 0 object stack)))

(defun %%new (instance
              args
              arglist-marshaller
              classfn
              method-index
              binding)
  (%%call (cffi:null-pointer)
          args arglist-marshaller classfn method-index
          (lambda (stack)
            (let ((new-object
                   (cffi:foreign-slot-value stack '(:union StackItem) 'ptr)))
              (set-object-binding classfn new-object binding)
              (setf (qobject-pointer instance) new-object))))
  (cache! instance))

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

(declaim (inline call-class-fun))
(defun call-class-fun (function method-index object stack)
  (with-fp-traps-masked
    (cffi:foreign-funcall-pointer
     function
     ()
     :short method-index
     :pointer object
     :pointer stack
     :void)))

(declaim (inline %%call))
(defun %%call (casted-instance-pointer
               args
               arglist-marshaller
               classfn
               method-index
               return-value-function)
  (funcall arglist-marshaller
           args
           (lambda (stack)
             (call-class-fun classfn method-index casted-instance-pointer
                             stack)
             (funcall return-value-function stack))))

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
                   stack
                   i
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
    (declare ((unsigned-byte 16) n))
    (named-lambda arglist-marshaller (arglist final-cont)
      (cffi:with-foreign-object (stack '(:union StackItem) n)
        (funcall thunk stack arglist final-cont)))))

(defun %interpret-call (allow-override-p instance method args)
  (let ((instance (full-resolve-this instance)))
    (funcall (resolve-call allow-override-p instance method args)
             instance
             args)))
