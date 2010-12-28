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

(defgeneric qobject-pointer (qobject))

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
             (qsubclassp (qobject-class cached) class))
        cached
        (if (cffi:null-pointer-p ptr)
            (make-instance 'null-qobject :class class)
            (let ((actual-class (or (when (qsubclassp class (find-qclass "QObject"))
                                      (instance-qclass ptr nil))
                                    class)))
              (make-instance 'qobject :class actual-class :pointer ptr))))))

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

(defvar *marshalling-tests* (make-hash-table))

(defmacro define-marshalling-test ((var type) &body body)
  `(setf (gethash ,type *marshalling-tests*)
         #'(lambda (,var) ,@body)))

(defun can-marshal-p (lisp-object <type>)
  (let ((slot (qtype-stack-item-slot <type>)))
    (alexandria:if-let ((test (gethash (qtype-interned-name
                                        (qtype-deconstify <type>))
                                       *marshalling-tests*)))
      (funcall test lisp-object)
      (cond ((typep lisp-object 'abstract-qobject)
             (and (eq slot 'class)
                  (qtypep lisp-object (qtype-class <type>))))
            ((let ((element-type (qlist-element-type <type>)))
               (and element-type
                    (alexandria:proper-list-p lisp-object)
                    (iter (for item in lisp-object)
                          (always (can-marshal-p item element-type))))))
            ((and (not (eq slot 'class))
                  (typep lisp-object
                         `(and ,(or (get slot 'lisp-type-for-stack-slot)
                                    (progn
                                      (warn "slot ~A not implemented"
                                            (qtype-stack-item-slot <type>))
                                      nil))
                               #+nil
                               ,(get (qtype-interned-name <type>)
                                     'lisp-type-for-qtype t)))))
            ((type= (qtype-deconstify <type>)
                    (with-cache () (qt::find-qtype "QByteArray")))
             (typep lisp-object 'string))))))

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
  (when (stringp thing)
    (setf thing (find-qtype thing)))
  (let ((kind (nth-value 2 (unbash thing))))
    (cond
      ((not (typep instance 'abstract-qobject)) nil)
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

(defun map-cpl-using-result (fun class initial-value)
  (labels ((recurse (c val)
             (let ((newval (funcall fun c val)))
               (map-qclass-superclasses
                (lambda (sub) (recurse sub newval))
                c))))
    (recurse class initial-value)))
