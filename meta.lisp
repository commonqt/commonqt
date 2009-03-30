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

(defclass dynamic-member ()
  ((name :initarg :name
         :accessor dynamic-member-name)
   (cached-arg-types :accessor dynamic-member-cached-arg-types)))

(defclass signal-member (dynamic-member)
  ((name :initarg :name
         :accessor dynamic-member-name)))

(defclass slot-member (dynamic-member)
  ((function :initarg :function
             :accessor dynamic-member-function)))

(defclass dynamic-object (qobject)
  ())

(defmethod print-object ((instance dynamic-object) stream)
  (print-unreadable-object (instance stream :type t :identity nil)
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

(defmethod initialize-instance ((instance dynamic-object) &key)
  (multiple-value-prog1
      (call-next-method)
    (let ((class (class-of instance)))
      (ensure-qt-class-caches class)
      (setf (qobject-class instance) (class-effective-class class)))))

(defmethod initialize-instance :around ((instance dynamic-object) &key)
  (multiple-value-prog1
      (call-next-method)
    (unless (cffi:pointerp (qobject-pointer instance))
      (error "INITIALIZE-INSTANCE of ~A failed to call Qt constructor"
             instance))))

(defclass qt-class (standard-class)
  ((qt-superclass :initarg :qt-superclass
                  :accessor class-qt-superclass)
   (signals :initarg :signals
            :accessor class-signals)
   (qt-slots :initarg :slots
             :accessor class-slots)
   (override-specs :initarg :override-specs
                   :accessor class-override-specs)
   (class-infos :initarg :class-infos
                :accessor class-class-infos)
   (effective-class :initform nil)
   (qmetaobject :initform nil)
   (smoke-generation :initform nil
                     :accessor class-smoke-generation)
   (member-table :accessor class-member-table)
   (overrides :accessor class-overrides)))

(defun default-overrides ()
  (let ((overrides (make-hash-table :test 'equal)))
    (setf (gethash "metaObject" overrides) 'metaobject-override)
    (setf (gethash "qt_metacall" overrides) 'qt_metacall-override)
    overrides))

(defmethod c2mop:validate-superclass
    ((class qt-class) (superclass t))
  nil)

(defmethod c2mop:validate-superclass
    ((class standard-class) (superclass qt-class))
  nil)

(defmethod c2mop:validate-superclass
    ((class qt-class) (superclass standard-class))
  (eq superclass (find-class 'dynamic-object)))

(defmethod c2mop:validate-superclass
    ((class qt-class) (superclass qt-class))
  t)

(defun parse-function (form)
  ;; this run-time use of COMPILE is a huge kludge.  We'd just want to hook
  ;; into the DEFCLASS expansion like slots and init functions can, but
  ;; those are special built-in features of DEFCLASS which meta classes
  ;; cannot implement for their own options.  Big oversight in the MOP IMNSHO.
  (typecase form
    ((or symbol function)
     form)
    ((cons (eql lambda) t)
     (compile nil form))
    ((cons (eql function) t)
     (eval form))))

(defun initialize-qt-class
    (class next-method &rest args
     &key qt-superclass direct-superclasses slots signals info override
     &allow-other-keys)
  (let* ((qt-superclass
          (if qt-superclass
              (destructuring-bind (name) qt-superclass
                (check-type name string)
                name)
              nil))
         (direct-superclasses
          (let ((qt-class (find-class 'qt-class))
                (standard-object (find-class 'standard-object))
                (dynamic-object (find-class 'dynamic-object)))
            (if (some (lambda (c) (typep c qt-class))
                      direct-superclasses)
                direct-superclasses
                (append (if (equal direct-superclasses (list standard-object))
                            nil
                            direct-superclasses)
                        (list dynamic-object)))))
         (slots
          (iter (for (name value) in slots)
                (collect (make-instance 'slot-member
                                        :name name
                                        :function (parse-function value)))))
         (signals
          (iter (for (name) in signals)
                (collect (make-instance 'signal-member
                                        :name name))))
         (class-infos
          (iter (for (name value) in info)
                (collect (make-class-info name value))))
         (override-specs
          (iter (for (method fun) in override)
                (collect (make-instance 'override-spec
                                        :method-name method
                                        :target-function fun)))))
    (apply next-method
           class
           :allow-other-keys t
           :direct-superclasses direct-superclasses
           :qt-superclass qt-superclass
           :slots slots
           :signals signals
           :class-infos class-infos
           :override-specs override-specs
           args)))

(defmethod initialize-instance ((instance qt-class) &rest args
                                &key &allow-other-keys)
  (apply #'initialize-qt-class instance #'call-next-method args))

(defmethod reinitialize-instance ((instance qt-class) &rest args)
  (apply #'initialize-qt-class instance #'call-next-method args))

(defun get-qt-class-member (qt-class id)
  (elt (class-member-table qt-class) id))

(defun make-override-table (specs)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (spec specs)
      (setf (gethash (override-spec-method-name spec) table)
            (override-spec-target-function spec)))
    table))

(defclass override-spec ()
  ((method-name :initarg :method-name
                :accessor override-spec-method-name)
   (target-function :initarg :target-function
                    :accessor override-spec-target-function)))

(defun merge-overrides (a b)
  (let ((c (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (setf (gethash k c) v )) a)
    (maphash (lambda (k v) (unless (gethash k c) (setf (gethash k c) v))) b)
    c))

(defmethod c2mop:finalize-inheritance :after ((object qt-class))
  (dolist (super (c2mop:class-direct-superclasses object))
    (unless (c2mop:class-finalized-p super)
      (c2mop:finalize-inheritance super)))
  (with-slots (qmetaobject qt-superclass member-table signals qt-slots
                           overrides)
      object
    (setf qmetaobject
          ;; clear out any old QMetaObject, so that ensure-metaobject will
          ;; set up a new one
          nil)
    (setf qt-superclass
          (or qt-superclass
              (class-qt-superclass
               (or (find-if (lambda (x) (typep x 'qt-class))
                            (c2mop:class-direct-superclasses object))
                   (error "No effective Qt class name declared for ~A"
                          object)))))
    (setf overrides (make-override-table (class-override-specs object)))
    (let ((supers (remove-if-not (lambda (super)
                                   (typep super 'qt-class))
                                 (c2mop:class-direct-superclasses object))))
      (if supers
          (dolist (super supers)
            (setf overrides
                  (merge-overrides overrides (class-overrides super))))
          (setf overrides (merge-overrides overrides (default-overrides)))))
    (setf member-table (concatenate 'vector signals qt-slots))))

(defun %qobject-metaobject ()
  (or *qobject-metaobject*
      (setf *qobject-metaobject*
            (let ((qobj (new (find-qclass "QObject"))))
              (prog1 (#_metaObject qobj)
                (delete-object qobj))))))

(defun ensure-qt-class-caches (qt-class)
  (check-type qt-class qt-class)
  (ensure-smoke)
  (with-slots (effective-class qmetaobject smoke-generation) qt-class
    (unless (and qmetaobject
                 effective-class
                 (eq smoke-generation *class-table*))
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
                   (qobject (find-qclass "QObject"))
                   (parent (cond
                             ((eq class qobject)
                              (%qobject-metaobject))
                             ((qsubtypep class qobject)
                              (#_staticMetaObject class))
                             (t
                              (null-qobject (find-qclass "QMetaObject"))))))
              (make-metaobject parent
                               (let ((name (class-name qt-class)))
                                 (format nil "~A::~A"
                                         (package-name (symbol-package name))
                                         (symbol-name name)))
                               (class-class-infos qt-class)
                               (mapcar #'convert-dynamic-member
                                       (class-signals qt-class))
                               (mapcar #'convert-dynamic-member
                                       (class-slots qt-class)))))
      ;; mark as fresh
      (setf (class-smoke-generation qt-class) *class-table*))))

(defun convert-dynamic-member (member)
  (make-slot-or-signal (dynamic-member-name member)))

(defun class-effective-class (qt-class)
  (ensure-qt-class-caches qt-class)
  (slot-value qt-class 'effective-class))

(defun class-qmetaobject (qt-class)
  (ensure-qt-class-caches qt-class)
  (slot-value qt-class 'qmetaobject))

(defmethod find-method-override ((object abstract-qobject) (method t))
  nil)

(defmethod find-method-override ((object qobject) (method t))
  (when (alexandria:starts-with #\~ (qmethod-name method))
;;;     (format t "dtor called on ~A~%" object)
;;;     (force-output)
    (note-deleted object)
    nil))

(defmethod find-method-override ((object dynamic-object) method)
  (or (gethash (qmethod-name method) (class-overrides (class-of object)))
      (call-next-method)))

(defvar *next-qmethod-trampoline* nil)
(defvar *next-qmethod* nil)

(defun call-next-qmethod (&rest args)
  (unless *next-qmethod-trampoline*
    (error "call-next-qmethod used outside of overriding method"))
  (funcall *next-qmethod-trampoline* args))

(defun get-next-qmethod ()
  (or *next-qmethod*
      (error "get-next-qmethod used outside of overriding method")))

(defun override (fun object method args)
  (let ((*next-qmethod* method)
        (*next-qmethod-trampoline*
         (lambda (new-args)
           (apply #'call-without-override object method (or new-args args)))))
    (apply fun object args)))

(defun metaobject-override (object)
  (class-qmetaobject (class-of object)))

(defun qt_metacall-override (object call id stack)
  (let ((new-id (call-next-qmethod)))
    (cond
      ((or (eql new-id -1)
           (not (eql (primitive-value call)
                     (primitive-value (#_InvokeMetaMethod
                                       (find-qclass "QMetaObject"))))))
       id)
      (t
       (let ((member
              (get-qt-class-member (class-of object) new-id)))
         (etypecase member
           (signal-member
            (#_activate (class-qmetaobject (class-of object))
                         object
                         id
                         stack)
            (int -1))
           (slot-member
            (check-type stack void**)
            (apply (dynamic-member-function member)
                   object
                   (unmarshal-slot-args member (primitive-value stack)))
            (int -1))))))))

(defun guess-stack-item-slot (x)
  (case x
    (:|int| 'int)
    (:|uint| 'uint)
    (:|bool| 'bool)
    (t (error "don't know how to unmarshal slot argument ~A" x))))

(defun ensure-dynamic-member-types (member)
  (with-slots (cached-arg-types) member
    (unless (slot-boundp member 'cached-arg-types)
      (setf cached-arg-types
            (mapcar (lambda (name)
                      (let* ((sym (intern name :keyword))
                             (slot (guess-stack-item-slot sym)))
                        (make-instance 'qtype
                                       :class nil
                                       :name name
                                       :interned-name sym
                                       :stack-item-slot slot
                                       :kind :stack
                                       :constp nil)))
                    (cl-ppcre:split
                     ","
                     (entry-arg-types (convert-dynamic-member member))))))
    cached-arg-types))

(defun unmarshal-slot-args (member argv)
  (iter (for type in (ensure-dynamic-member-types member))
        (for i from 1)
        (collect (unmarshal type (cffi:mem-aref argv :pointer i)))))

(defmethod new ((class qt-class) &rest args)
  (apply #'new
         (make-instance class :pointer :unborn)
         args))

(defmethod new ((class symbol) &rest args)
  (apply #'new (find-class class) (when args (list :qt-ctor-args args))))

(defclass class-info ()
  ((key :initarg :key
        :accessor entry-key)
   (value :initarg :value
          :accessor entry-value)))

(defclass slot-or-signal ()
  ((name :initarg :name
         :accessor entry-name)
   (full-name :initarg :full-name
              :accessor entry-full-name)
   (arg-types :initarg :arg-types
              :accessor entry-arg-types)
   (reply-type :initarg :reply-type
               :accessor entry-reply-type)))

(defun make-class-info (key value)
  (make-instance 'class-info :key key :value value))

(defun make-slot-or-signal (str)
  (let ((str (#_data
              (#_normalizedSignature (find-qclass "QMetaObject") str))))
    (cl-ppcre:register-groups-bind (a b c d)
        ("^(([\\w,<>:]*)\\s+)?([^\\s]*)\\((.*)\\)" str)
      (declare (ignore a))
      (make-instance 'slot-or-signal
                     :name c
                     :full-name (concatenate 'string c "(" d ")")
                     :arg-types d
                     :reply-type (if (or (null b) (equal b "void")) "" b)))))

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
  (cffi:with-foreign-object (argv '|union StackItem| (1+ (length args)))
    (cffi:with-foreign-object (stack '|union StackItem| (1+ (length args)))
      (iter (for i from 1 to (length args))
            (setf (cffi:mem-aref argv :pointer i)
                  (cffi:mem-aref stack '|union StackItem| i)))
      (labels ((iterate (i rest-types rest-args)
                 (cond
                   (rest-args
                    (marshal (car rest-args)
                             (car rest-types)
                             (cffi:mem-aref stack '|union StackItem| i)
                             (lambda ()
                               (iterate (1+ i)
                                        (cdr rest-types)
                                        (cdr rest-args)))))
                   (t
                    (funcall fun argv)))))
        (iterate 1 types args)))))

(defun emit-signal (object name &rest args)
  (let* ((meta (class-qmetaobject (class-of object)))
         (id (#_indexOfSignal meta name))
         (offset (#_methodOffset meta)))
    (unless (>= id 0)
      (error "no such signal ~A on ~A" name object))
    (apply #'values
           (call-with-signal-marshalling
            (lambda (stack)
              (list (#_activate meta object id (void** stack))))
            (ensure-dynamic-member-types
             (get-qt-class-member (class-of object) (- id offset)))
            args))))
