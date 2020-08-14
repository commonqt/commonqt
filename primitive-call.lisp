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
  (if (some #'keywordp forms)
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
           (return (values types clean-forms)))))
      (values nil forms)))

(deftype cont-fun ()
  `(function * (values t &optional)))

(defun make-optimized (instance
                       &key instance-resolver args resolver)
  (flet ((number-of-non-constantp (list)
           (count-if-not #'constantp list)))
    (multiple-value-bind (fix-types args) (parse-optimized-call-args args)
      (let ((argsyms (make-symbols 'arg (length args)))
            (sigsyms (make-symbols 'sig (number-of-non-constantp args)))
            (instance-qclass-sym (gensym "INSTANCE-QCLASS"))
            (instance-extra-sig-sym (gensym "INSTANCE-EXTRA-SIG"))
            (instance-sym (gensym "INSTANCE")))
        ;; FIXME: check evaluation order
        `(multiple-value-bind (,instance-sym
                               ,instance-qclass-sym
                               ,instance-extra-sig-sym)
             ,(funcall instance-resolver instance)
           (declare (type (unsigned-byte 24) ,instance-qclass-sym))
           (let (,@(iter (for arg in args)
                     (for sym in argsyms)
                     (unless (constantp arg)
                       (collect `(,sym ,arg)))))
             (let* ((types ',fix-types)
                    (args ,(if (zerop (number-of-non-constantp args))
                               `',(mapcar #'eval args)
                               `(list*
                                 ,@(loop for (arg . rest) on args
                                         for argsym in argsyms
                                         collect
                                         (if (constantp arg)
                                             arg
                                             argsym)
                                         if (zerop (number-of-non-constantp rest))
                                         collect `',(mapcar #'eval rest)
                                         and
                                         do (loop-finish)))))
                    (instance ,instance-sym)
                    ,@(loop with sigs = sigsyms
                            for arg in args
                            for argsym in argsyms
                            unless (constantp arg)
                            collect `(,(pop sigs) (signature-type ,argsym))))
               (declare (dynamic-extent args)
                        (optimize (safety 0)))
               (cached-values-bind (fun) ,resolver
                   ((,instance-qclass-sym :hash t)
                    (,instance-extra-sig-sym)
                    ,@(loop for sig in sigsyms
                            collect `(,sig :hash sxhash)))
                 (declare (type cont-fun fun))
                 (funcall fun ,instance-sym args)))))))))

(defmacro optimized-call (allow-override-p instance method &rest args)
  (make-optimized instance
    :instance-resolver #'compile-time-resolve-this
    :args args
    :resolver `(resolve-call ,allow-override-p instance ,method args types)))

(defmacro optimized-new (class-or-instance &rest args)
  (make-optimized class-or-instance
    :instance-resolver #'compile-time-resolve-ctor-this
    :args args
    :resolver `(resolve-new instance args types)))

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
             (funcall return-value-function stack t))))

(defun resolve-call (allow-override-p instance method args fix-types)
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
           (multiple-value-bind (castfn <from> <to>)
               (resolve-cast <from> (qmethod-class method))
             (let ((cont
                     (if precompiled-override
                         (lambda (actual-instance args)
                           (block nil
                             (catch 'stop-overriding-tag
                               (return (override precompiled-override
                                                 actual-instance method args)))
                             (%%call (perform-cast actual-instance castfn <from> <to>)
                                     args
                                     arglist-marshaller
                                     classfn
                                     method-index
                                     return-value-function)))
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
         (class-name (qclass-name class))
         (colon (position #\: class-name :from-end t))
         ;; KLUDGE: Some classes have constructors without namespace prefix
         ;; e.g. Phonon does that. Might be smoke's fault.
         (constructor (if colon
                          (subseq class-name (1+ colon))
                          class-name))
         (method
           (qclass-find-applicable-method class
                                          constructor
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
          (lambda (stack delete)
            (declare (ignore delete))
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
    (dynamic-object
     (values instance
             (qobject-class instance)
             (class-generation (class-of instance))))
    (qobject
     (values instance (qobject-class instance) :instance))
    (integer
     (values instance instance :static))
    (symbol
     (let ((qclass (class-effective-class (find-class instance))))
       (values qclass qclass :static)))
    (qt-class
     (let ((qclass (class-effective-class instance)))
       (values qclass qclass :static)))
    (string
     (let ((qclass (find-qclass instance)))
       (values qclass qclass :static)))))

(defun compile-time-resolve-this (instance)
  (etypecase instance
    (string `(let ((qclass (with-cache ()
                             (find-qclass ,instance))))
               (values qclass qclass :static)))
    ((cons (eql quote) (cons symbol null))
     `(let ((qclass (class-effective-class (find-class ,instance))))
        (values qclass qclass :static)))
    (t `(full-resolve-this ,instance))))

(defun full-resolve-ctor-this (instance)
  (etypecase instance
    (dynamic-object
     (values instance
             (qobject-class instance)
             (class-generation (class-of instance))))
    (qobject
     (values instance (qobject-class instance) :instance))
    (integer
     (values
      (make-instance 'qobject :class instance :pointer :unborn)
      instance
      :instance))
    (string
     (let ((qclass (find-qclass instance) ))
       (values (make-instance 'qobject :class qclass :pointer :unborn)
               qclass
               :instance)))
    (qt-class
     (values (make-instance instance :pointer :unborn)
             (class-effective-class instance)
             (class-generation instance)))))

(defun compile-time-resolve-ctor-this (instance)
  (etypecase instance
    (string
     `(let ((qclass (with-cache () (find-qclass ,instance))))
        (values (make-instance 'qobject
                               :class qclass
                               :pointer :unborn)
                qclass
                :instance)))
    (t `(full-resolve-ctor-this ,instance))))

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
    (declare (type (unsigned-byte 16) n))
    (named-lambda arglist-marshaller (arglist final-cont)
      (cffi:with-foreign-object (stack '(:union StackItem) n)
        (funcall thunk stack arglist final-cont)))))

(defun %interpret-call (allow-override-p instance method args)
  (let ((instance (full-resolve-this instance)))
    (funcall (resolve-call allow-override-p instance method args nil)
             instance
             args)))
