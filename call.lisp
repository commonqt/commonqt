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

(defmacro with-callback-restart (&body body)
  `(restart-case
       (progn ,@body)
     (abort ()
       :report (lambda (stream)
                 (write-string "Abort smoke callback" stream))
       0)))

(defun %deletion-callback (obj)
  (with-callback-restart
    (let ((object (pointer->cached-object obj)))
      (when object
        (note-deleted object)))))

(defun unmarshal-args (stack <method>)
  (let ((index 0))
    (iter
      (map-qmethod-argument-types
       (lambda (type)
         (collect (unmarshal type
                             (cffi:mem-aref stack
                                            '|union StackItem|
                                            (incf index)))))
       <method>)
      (finish))))

(defun %method-invocation-callback (smoke method-idx obj stack)
  (with-callback-restart
    (let* ((<module> (module-number smoke))
           (object (pointer->cached-object obj))
           (<method> (bash method-idx <module> +method+))
           (fun (and object (find-method-override object <method>))))
      (if fun
          (let* ((args (unmarshal-args stack <method>))
                 (result (override fun object <method> args))
                 (rtype (qmethod-return-type <method>)))
            (unless (qtype-void-p rtype)
              (marshal result rtype stack (lambda ())))
            1)
          0))))

(defun %dynamic-invocation-callback (smoke obj
                                     method-id
                                     override-id stack)
  (with-callback-restart
    (let* ((<module> (module-number smoke))
           (object (pointer->cached-object obj))
           (<method> (bash method-id <module> +method+))
           (override (and object
                          (find-dynamic-method-override object
                                                        override-id))))
      (if override
          ;; Instead of explicitly calling the next method
          ;; we can just return 0 from here and Qt will call the next method
          ;; (stop-overriding) can just throw 0 here
          (catch 'stop-overriding-tag
            (let* ((args (unmarshal-args stack <method>))
                   (result (override (spec-function override)
                                     object (name override) args))
                   (rtype (qmethod-return-type <method>)))
              (unless (qtype-void-p rtype)
                (marshal result rtype stack (lambda ())))
              1))
          0))))

(defun %child-callback (added obj)
  (with-callback-restart
    (let ((object (pointer->cached-object obj)))
      (when object
        (if (zerop added)
            (note-child-removed object)
            (note-child-added object))))))

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
  (block nil
    (labels ((find-applicable (class)
               (map-class-methods-named
                (lambda (method)
                  (when (method-applicable-p method args fix-types)
                    (return method)))
                class method-name))
             (recurse (class)
               (or (find-applicable class)
                   (mapc #'recurse (list-qclass-superclasses class)))))
      (recurse class))))

(defun method-applicable-p (method args &optional fix-types)
  (block nil
    (map-qmethod-argument-types
     (lambda (arg-type)
       (let ((fix-type (pop fix-types)))
         (unless (and (or (not fix-type)
                          (eq (qtype-interned-name arg-type)
                              fix-type))
                      (can-marshal-p (pop args) arg-type))
           (return))))
     method)
    (null args)))

(defun qtypep (instance thing)
  (when (stringp thing)
    (setf thing (find-qtype thing)))
  (let ((kind (nth-value 2 (unbash thing))))
    (cond
      ((not (typep instance 'abstract-qobject)) nil)
      ((eql kind +class+) (qsubclassp (qobject-class instance) thing))
      ((eql kind +type+) (qtypep instance (qtype-class thing)))
      (t (error "Not a type or class: ~A" thing)))))

(defun qsubclassp (a b)
  (block nil
    (or (eql a b)
        (map-qclass-direct-superclasses
         (lambda (superclass)
           (when (qsubclassp superclass b)
             (return t)))
         a))))

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
      (remhash addr *strongly-cached-objects*)
      (map-casted-object-pointer
       (lambda (ptr)
         (let ((super-addr (cffi:pointer-address ptr)))
           (when (/= super-addr addr)
             (remhash super-addr *weakly-cached-objects*))))
       (qobject-class object)
       (qobject-pointer object)))
    (setf (qobject-deleted object) t)))

(defvar *report-memory-leaks* nil)

(defun cancel-finalization (object)
  (check-type object abstract-qobject)
  (when *report-memory-leaks*
    (tg:cancel-finalization object)))

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

(defun map-qclass-precedence-list (fun class)
  (labels ((recurse (c)
             (funcall fun c)
             (map-qclass-direct-superclasses #'recurse c)))
    (recurse class)))

(defun map-casted-object-pointer (fun <class> pointer)
  "Cast an object to each of its superclasses and call a function on
   the resulting pointer"
  (labels ((recurse (pointer <from> <to>)
             (let ((casted (%cast pointer <from> <to>)))
               (funcall fun casted)
               (map-qclass-direct-superclasses
                (lambda (<super>)
                  (recurse casted <to> <super>))
                <to>))))
    (map-qclass-direct-superclasses
     (lambda (<super>)
       (recurse pointer <class> <super>))
     <class>)))
