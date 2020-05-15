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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +index-bits+  16)
  (defconstant +module-bits+  4)
  (defconstant +kind-bits+    2))

(deftype index         () `(unsigned-byte ,+index-bits+))
(deftype module-number () `(unsigned-byte ,+module-bits+))
(deftype kind          () `(unsigned-byte ,+kind-bits+))

(deftype tagged ()
  `(unsigned-byte ,(+ +index-bits+ +module-bits+ +kind-bits+)))

(deftype module-iterator () '(integer -1 #.(expt 2 +module-bits+)))
(deftype index-iterator  () '(integer -1 #.(expt 2 +index-bits+)))

(deftype ambiguous-method-index () `(signed-byte ,(1+ +index-bits+)))

;;;;
;;;; Module
;;;;

(defvar *n-modules* 0)
(declaim (type module-iterator *n-modules*))

(defvar *module-table*
  (make-array (ash 1 +module-bits+) :initial-element nil))

(defvar *module-data-table*
  (make-array (ash 1 +module-bits+) :initial-element nil))

(declaim (type (simple-array t (#.(expt 2 +module-bits+)))
               *module-table*
               *module-data-table*))

(declaim (inline module-ref))
(defun module-ref (i) (svref *module-table* i))

(declaim (inline data-ref))
(defun data-ref (i) (svref *module-data-table* i))

;;;;
;;;; Bit bashing
;;;;

;;;; Smoke uses tables of classes, methods, types, and "methodmap"s,
;;;; each indexed by a 16 bit integer.  There can be multiple smoke
;;;; instances ("modules") having separate tables, interlinked through
;;;; "external classes".
;;;;
;;;; We use the following encoding scheme to represent references
;;;; into this kind of meta data as a 22 bit integer:
;;;;
;;;;    0000000000000001000100      = (class number 1 in the second module)
;;;;    <--------------><--><>
;;;;       |             |   |
;;;;       |             |  2 bit type
;;;;       |             |
;;;;       |        4 bit module index
;;;;      16 bit index
;;;;
;;;; Properties:
;;;;   - no CLOS object caching, no memory overhead
;;;;   - can just compare references using EQL
;;;;   - fits into a fixnum
;;;;   - Index ordering within a module and type is preserved, so that
;;;;     binary search in the tables works for references as well as indexes.
;;;;
;;;; Note:
;;;;   - Since only a fixed number of bits is reserved for the module
;;;;     index, we can't load an arbitrary number of smoke modules at run
;;;;     time.  There are only few such modules in kdebindings though, so
;;;;     this isn't really a limitation.  If we ever need more than 16
;;;;     modules, we can increase +module-bits+ a little.

(defconstant +class+ 0)
(defconstant +method+ 1)
(defconstant +methodmap+ 2)
(defconstant +type+ 3)

(declaim (inline module-number))
(defun module-number (smoke)
  (position smoke
            *module-table*
            :test #'cffi:pointer-eq
            :end *n-modules*))

(defun named-module-number (name)
  (position name
            *module-data-table*
            :key (lambda (data)
                   (and data (data-name data)))
            :test #'string=))

#-qt::debug (declaim (inline bash))
(defun bash (idx module-number kind)
  (declare (type index idx))
  (declare (type module-number module-number))
  (declare (type kind kind))
  (logior kind
          (ash (logior module-number
                       (ash idx +module-bits+))
               +kind-bits+)))

#-qt::debug (declaim (inline ldb-module))
(defun ldb-module (x)
  (declare (type tagged x))
  (ldb (byte +module-bits+ +kind-bits+) x))

#-qt::debug (declaim (inline ldb-kind))
(defun ldb-kind (x)
  (declare (type tagged x))
  (ldb (byte +kind-bits+ 0) x))

#-qt::debug (declaim (inline unbash))
(defun unbash (x)
  (declare (type tagged x))
  (values (ldb (byte 16 (+ +module-bits+ +kind-bits+)) x)
          (ldb-module x)
          (ldb (byte +kind-bits+ 0) x)))

#-qt::debug (declaim (inline unbash*))
(defun unbash* (x expected-kind)
  (declare (type tagged x)
           #-qt::debug (ignore expected-kind))
  (multiple-value-bind (idx <module> kind)
      (unbash x)
    #+qt::debug (assert (eql kind expected-kind))
    (values idx <module> kind)))


;;;;
;;;; Names
;;;;

(defun %find-name (smoke str)
  (sw_find_name smoke str))


;;;;
;;;; Classes
;;;;

(defun map-classes (fun &optional allow-external)
  (iter (for <module> below *n-modules*)
        (declare (type module-iterator <module>))
        (map-classes-in-module fun <module> allow-external)))

(defun map-classes-in-module (fun <module> &optional allow-external)
  (let ((n (data-nclasses (data-ref <module>))))
    (iter (for i from 1 to n)
          (declare (type index-iterator i))
	  (let ((<class> (bash i <module> +class+)))
	    (unless (and (qclass-external-p <class>) (not allow-external))
	      (funcall fun <class>))))))

(declaim (inline qclass-struct))
(defun qclass-struct (<class>)
  (declare (type tagged <class>))
  (multiple-value-bind (idx <module>)
      (unbash* <class> +class+)
    #+qt::debug (assert (<= 0 idx (data-nclasses (data-ref <module>))))
    (cffi:mem-aptr (data-classes (data-ref <module>))
                   '(:struct qClass)
                   idx)))

(defun qclass-name (<class>)
  (cffi:foreign-slot-value (qclass-struct <class>) '(:struct qClass) 'classname))

(defun qclass-external-p (<class>)
  (plusp
   (cffi:foreign-slot-value (qclass-struct <class>) '(:struct qClass) 'external)))

(defun resolve-external-qclass (<class>)
  (if (qclass-external-p <class>)
      (find-qclass (qclass-name <class>))
      <class>))

(defun instance-qclass (ptr &optional (errorp t))
  (or (cffi:with-foreign-object (&smoke :pointer)
        (cffi:with-foreign-object (&index :short)
          (sw_id_instance_class ptr &smoke &index)
          (let ((smoke (cffi:mem-ref &smoke :pointer)))
            (unless (cffi:null-pointer-p smoke)
              (bash (cffi:mem-ref &index :short)
                    (module-number smoke)
                    +class+)))))
      (when errorp
        (error "Class not found for ~S" ptr))))

(defun find-qclass (name &optional (errorp t))
  (etypecase name
    (integer
     (assert (eql (ldb-kind name) +class+))
     name)
    (string
     (or (cffi:with-foreign-object (&smoke :pointer)
           (cffi:with-foreign-object (&index :short)
             (sw_find_class name &smoke &index)
             (let ((smoke (cffi:mem-ref &smoke :pointer)))
               (unless (cffi:null-pointer-p smoke)
                 (bash (cffi:mem-ref &index :short)
                       (module-number smoke)
                       +class+)))))
         (when errorp
           (error "Class not found: ~A" name))))))

(defun find-qclass-in-module (<module> name &optional (allow-external t))
  (declare (type module-number <module>))
  (let ((index (the index-iterator
                 (sw_id_class (module-ref <module>)
                              name
                              (if allow-external 1 0)))))
    (and (plusp index) (bash index <module> +class+))))

(defun map-method-in-class-module (function <class> method-name)
  (multiple-value-bind (class-id <module>) (unbash* <class> +class+)
    (let* ((name-ref (%find-name (module-ref <module>) method-name))
           (smoke (data-ref <module>))
           (methods (data-methods smoke))
           (nmethods (data-nmethods smoke)))
      (loop for id below nmethods
            for start-method = (cffi:mem-aptr methods '(:struct qMethod) id)
            for start-class = (cffi:foreign-slot-value start-method '(:struct qMethod) 'classid)
            when (and (eql start-class class-id)
                      (eql (cffi:foreign-slot-value start-method '(:struct qMethod) 'name) name-ref))
            do
            (funcall function (bash id <module> +method+))
            (loop for next-id from (1+ id) below nmethods
                  for method = (cffi:mem-aptr methods '(:struct qMethod) next-id)
                  for class = (cffi:foreign-slot-value method '(:struct qMethod) 'classid)
                  while (and (eql class class-id)
                             (eql (cffi:foreign-slot-value method '(:struct qMethod) 'name) name-ref))
                  do (funcall function (bash next-id <module> +method+)))
            (loop-finish)))))

(defmacro deflistify (list-name map-name &rest args)
  `(defun ,list-name (,@args)
     (iter
       (,map-name (lambda (x) (collect x)) ,@args)
       (finish))))

(defun format-reference (stream arg foo bar)
  (declare (ignore foo bar))
  (multiple-value-bind (id <module> kind)
      (unbash arg)
    (format stream "~D <~D,~D,~D>" arg id <module> kind)))

(declaim (inline map-qclass-direct-superclasses))
(defun map-qclass-direct-superclasses (fun <class>)
  (let* ((<module> (ldb-module <class>))
         (parents (the index-iterator
                    (cffi:foreign-slot-value (qclass-struct <class>)
                                             '(:struct qClass)
                                             'parents)))
         (inheritancelist (data-inheritancelist (data-ref <module>))))
    (iter (for i from parents)
          (declare (type index-iterator i))
          (let ((classid (the index-iterator
                           (cffi:mem-aref inheritancelist :short i))))
            (while (plusp classid))
	    (funcall fun (or (resolve-external-qclass
			      (bash classid <module> +class+))
			     (error "Failed to resolve superclass: ~/qt:format-reference/"
				    (bash classid <module> +class+))))))))

(deflistify list-qclass-superclasses map-qclass-direct-superclasses
  <class>)

(defun qclass-flags (<class>)
  (cffi:foreign-slot-value (qclass-struct <class>) '(:struct qClass) 'flags))

(macrolet ((deftest (name mask)
             `(defun ,name (<class>)
                (logtest ,mask (qclass-flags <class>)))))
  (deftest qclass-constructor-p #x01)
  (deftest qclass-deepcopy-p #x02)
  (deftest qclass-virtual-p #x04)
  (deftest qclass-namespace-p #x08)
  (deftest qclass-undefined-p #x10))

(defun list-qclass-flags (<class>)
  (let ((x '()))
    (when (qclass-constructor-p <class>) (push :constructor x))
    (when (qclass-deepcopy-p <class>) (push :deepcopy x))
    (when (qclass-virtual-p <class>) (push :virtual x))
    (when (qclass-namespace-p <class>) (push :namespace x))
    (when (qclass-undefined-p <class>) (push :undefined x))
    x))

(declaim (inline qclass-trampoline-fun))
(defun qclass-trampoline-fun (<class>)
  (declare (type tagged <class>))
  (cffi:foreign-slot-value (qclass-struct <class>) '(:struct qClass) 'classfn))

(declaim (inline qclass-enum-fun))
(defun qclass-enum-fun (<class>)
  (declare (type tagged <class>))
  (cffi:foreign-slot-value (qclass-struct <class>) '(:struct qClass) 'enumfn))


;;;;
;;;; MethodMap
;;;;

(defun methodmap-struct (<methodmap>)
  (multiple-value-bind (idx <module>)
      (unbash* <methodmap> +methodmap+)
    #+qt::debug (assert (<= 0 idx (data-nmethodmaps (data-ref <module>))))
    (cffi:mem-aptr (data-methodmaps (data-ref <module>))
                   '(:struct MethodMap)
                   idx)))

(declaim (ftype (function (tagged) tagged) methodmap-class))
(declaim (inline methodmap-class))
(defun methodmap-class (<methodmap>)
  (bash (cffi:foreign-slot-value (methodmap-struct <methodmap>)
                                 '(:struct MethodMap)
                                 'classid)
        (ldb-module <methodmap>)
        +class+))

(declaim (inline map-methodmap-methods))
(defun map-methodmap-methods (fun <methodmap>)
  (declare (type tagged <methodmap>))
  (let ((<module> (ldb-module <methodmap>))
        (methodid (cffi:foreign-slot-value (methodmap-struct <methodmap>)
                                           '(:struct MethodMap)
                                           'methodid)))
    (declare (type ambiguous-method-index methodid))
    (if (plusp methodid)
        (funcall fun (bash methodid <module> +method+))
        (let ((ambiguous-methods
               (data-ambiguousMethodList (data-ref <module>))))
          (iter (for i from (- methodid))
                (declare (type index i))
                (let ((id (cffi:mem-aref ambiguous-methods :short i)))
                  (while (plusp id))
                  (funcall fun (bash id <module> +method+))))))))

(deflistify list-methodmap-methods map-methodmap-methods
  <methodmap>)

(defun name-ref (<module> idx)
  (declare (type module-number <module>))
  (declare (type index idx))
  (cffi:mem-aref (data-methodnames (data-ref <module>))
                 '(:string :encoding :ascii)
                 idx))

(declaim (inline methodmap-name-index))
(defun methodmap-name-index (<methodmap>)
  (declare (type tagged <methodmap>))
  (cffi:foreign-slot-value (methodmap-struct <methodmap>)
                           '(:struct MethodMap)
                           'name))

(declaim (inline methodmap-name))
(defun methodmap-name (<methodmap>)
  (declare (type tagged <methodmap>))
  (name-ref (ldb-module <methodmap>)
            (the index (methodmap-name-index <methodmap>))))

(defun find-methodmap (<class> name)
  (multiple-value-bind (classid <module>)
      (unbash* <class> +class+)
    (let* ((smoke (module-ref <module>))
           (index
            (the index (sw_id_method smoke classid (%find-name smoke name)))))
      (if (zerop index)
          nil
          (bash index <module> +methodmap+)))))


;;;;
;;;; Methods
;;;;

(defun map-methods (fun)
  (iter (for <module> below *n-modules*)
        (declare (type module-iterator <module>))
        (map-methods-in-module fun <module>)))

(defun map-methods-in-module (fun <module>)
  (let ((n (data-nmethods (data-ref <module>))))
    (iter (for i from 1 below n)
          (declare (type index-iterator i))
          (funcall fun (bash i <module> +method+)))))

(declaim (inline qmethod-struct))
(defun qmethod-struct (<method>)
  (multiple-value-bind (idx <module>)
      (unbash* <method> +method+)
    #+qt::debug (assert (<= 0 idx (data-nmethods (data-ref <module>))))
    (cffi:mem-aptr (data-methods (data-ref <module>))
                   '(:struct qMethod)
                   idx)))

(declaim (inline qmethod-class))
(defun qmethod-class (<method>)
  (declare (type tagged <method>))
  (bash (cffi:foreign-slot-value (qmethod-struct <method>)
                                 '(:struct qMethod)
                                 'classid)
        (ldb-module <method>)
        +class+))

(declaim (inline qmethod-name-index))
(defun qmethod-name-index (<method>)
  (declare (type tagged <method>))
  (cffi:foreign-slot-value (qmethod-struct <method>)
                           '(:struct qMethod)
                           'name))

(declaim (inline qmethod-name))
(defun qmethod-name (<method>)
  (declare (type tagged <method>))
  (name-ref (ldb-module <method>)
            (the index (qmethod-name-index <method>))))

(defun qmethod-flags (<method>)
  (cffi:foreign-slot-value (qmethod-struct <method>) '(:struct qMethod) 'flags))

(macrolet ((deftest (name mask)
             `(defun ,name (<method>)
                (logtest ,mask (qmethod-flags <method>)))))
  (deftest qmethod-static-p #x01)
  (deftest qmethod-const-p #x02)
  (deftest qmethod-copyctor-p #x04)
  (deftest qmethod-internal-p #x08)
  (deftest qmethod-enum-p #x10)
  (deftest qmethod-ctor-p #x20)
  (deftest qmethod-dtor-p #x40)
  (deftest qmethod-protected-p #x80)
  (deftest qmethod-virtual-p #x400))

(defun list-qmethod-flags (<method>)
  (remove-if-not (lambda (fun)
		   (funcall fun <method>))
		 '(qmethod-static-p
		   qmethod-const-p
		   qmethod-copyctor-p
		   qmethod-internal-p
		   qmethod-enum-p
		   qmethod-ctor-p
		   qmethod-dtor-p
		   qmethod-protected-p)))

(defun qmethod-return-type (<method>)
  (bash (cffi:foreign-slot-value (qmethod-struct <method>)
				 '(:struct qMethod)
				 'ret)
	(ldb-module <method>)
	+type+))

(defun qmethod-argument-number (<method>)
  (cffi:foreign-slot-value (qmethod-struct <method>) '(:struct qMethod)
                           'numargs))

(declaim (inline map-qmethod-argument-types))
(defun map-qmethod-argument-types (fun <method>)
  (let* ((<module> (ldb-module <method>))
         (argumentlist (data-argumentlist (data-ref <module>))))
    (cffi:with-foreign-slots
        ((args numargs) (qmethod-struct <method>) (:struct qMethod))
      (declare (type index-iterator args numargs))
      (if (plusp numargs)
          (iter (for i from args)
                (declare (type index i))
                (repeat numargs)
                (funcall fun
                         (bash (cffi:mem-aref argumentlist :short i)
                               <module>
                               +type+)))
          nil))))

(deflistify list-qmethod-argument-types map-qmethod-argument-types
  <method>)

(defun qmethod-classfn-index (<method>)
  (cffi:foreign-slot-value (qmethod-struct <method>)
                           '(:struct qMethod)
                           'methodForClassFun))

;;;;
;;;; Type
;;;;

;; fixme: zerop flags; zerop classid

(defun qtype-void-p (<type>)
  (zerop (unbash <type>)))

(defun map-types (fun)
  (iter (for <module> below *n-modules*)
        (declare (type module-iterator <module>))
        (map-types-in-module fun <module>)))

(defun map-types-in-module (fun <module>)
  (let ((n (data-ntypes (data-ref <module>))))
    (iter (for i from 1 to n)
          (declare (type index i))
	  (funcall fun (bash i <module> +type+)))))

(declaim (inline qtype-struct))
(defun qtype-struct (<type>)
  (declare (type tagged <type>))
  (multiple-value-bind (idx <module>)
      (unbash* <type> +type+)
    #+qt::debug (assert (<= 0 idx (data-ntypes (data-ref <module>))))
    (cffi:mem-aptr (data-types (data-ref <module>))
                   '(:struct qType)
                   idx)))

(declaim (inline qtype-class))
(defun qtype-class (<type>)
  (declare (type tagged <type>))
  (resolve-external-qclass
   (bash (cffi:foreign-slot-value (qtype-struct <type>)
				  '(:struct qType)
				  'classid)
	 (ldb-module <type>)
	 +class+)))

(declaim (inline qtype-name))
(defun qtype-name (<type>)
  (declare (type tagged <type>))
  (cffi:foreign-slot-value (qtype-struct <type>) '(:struct qType) 'name))

(defun qtype-interned-name (<type>)
  (intern (qtype-name <type>) :keyword))

(defun qtype-flags (<type>)
  (cffi:foreign-slot-value (qtype-struct <type>) '(:struct qType) 'flags))

(defun qtype-stack-item-slot (<type>)
  (elt #(ptr bool char uchar short ushort int uint long ulong float double
         enum class)
       (logand #xf (qtype-flags <type>))))

(defun qtype-kind (<type>)
  (case (logand #x30 (qtype-flags <type>))
    (#x10 :stack)
    (#x20 :pointer)
    (#x30 :reference)))

(defun qtype-constp (<type>)
  (logtest #x40 (qtype-flags <type>)))

(defun find-qtype (name &optional <module>)
  (loop for i from (or <module> 0) to (or <module> (1- *n-modules*))
        for index = (sw_id_type (module-ref i) name)
        when (plusp index) return (bash index i +type+)))

;;;;
;;;; Classes (cont. from above, now that inlined function are there)
;;;;

(defun %find-any-methodmap-for-class (<class>)
  (declare (type tagged <class>))
  ;; Note: The way the comparison is currently written depends on <class>
  ;; order being the same as index order within the same module.
  (let* ((<module> (ldb-module <class>))
         (from 1)
         (to (data-nmethodmaps (data-ref <module>))))
    (declare (type index-iterator from to))
    (iter (while (<= from to))
          (let* ((current-index
                  (truncate (+ from to) 2))
                 (current-<methodmap>
                  (bash current-index <module> +methodmap+))
                 (current-<class>
                  (methodmap-class current-<methodmap>)))
            (cond
              ((eql current-<class> <class>)
               (return current-<methodmap>))
              ((> current-<class> <class>)
               (setf to (1- current-index)))
              (t
               (setf from (1+ current-index))))))))

(declaim (inline map-class-methodmaps))
(defun map-class-methodmaps (fun <class>)
  (let ((any (%find-any-methodmap-for-class <class>)))
    (when any
      (multiple-value-bind (first-idx <module>)
			   (unbash any)
	(macrolet
	    ((% (from offset)
	       `(iter (for idx ,from (+ first-idx ,offset))
                      (declare (type index-iterator idx))
		      (let ((<methodmap> (bash idx <module> +methodmap+)))
			(while (eql <class> (methodmap-class <methodmap>)))
			(funcall fun <methodmap>)))))
	  (% from 0)
	  (% downfrom -1))))))

(deflistify list-qclass-methodmaps map-class-methodmaps
  <class>)

(defun %find-name-index-range (<module> method-name)
  (let* ((str-length (length method-name))
         (from (the index (%find-name (module-ref <module>) method-name)))
         (to (data-nmethodnames (data-ref <module>))))
    (declare (type index-iterator to))
    (when (plusp from)
      (values from
              (iter (for current-index from (1+ from) below to)
                    (let* ((current-name (name-ref <module> current-index))
                           (mismatch (mismatch current-name method-name)))
                      (while (and (eql mismatch str-length)
                                  (find (char current-name str-length) "?#$")))
                      (finally (return (1- current-index)))))))))

(defun %find-any-methodmap-for-class-and-name-range (<class> min max)
  (declare (type tagged <class>))
  (let* ((<module> (ldb-module <class>))
         (from 1)
         (to (data-nmethodmaps (data-ref <module>))))
    (declare (type index-iterator from to))
    (iter (while (<= from to))
          (let* ((current-index
                  (truncate (+ from to) 2))
                 (current-<methodmap>
                  (bash current-index <module> +methodmap+))
                 (current-<class>
                  (methodmap-class current-<methodmap>))
                 (current-name-index
                  (methodmap-name-index current-<methodmap>)))
            (cond
              ((eql current-<class> <class>)
               (cond
                 ((<= min current-name-index max)
                  (return current-<methodmap>))
                 ((> current-name-index max)
                  (setf to (1- current-index)))
                 (t
                  (setf from (1+ current-index)))))
              ((> current-<class> <class>)
               (setf to (1- current-index)))
              (t
               (setf from (1+ current-index))))))))

(defun map-class-methodmaps-named (fun <class> method-name)
  (declare (type tagged <class>))
  (multiple-value-bind (min max)
      (%find-name-index-range (ldb-module <class>) method-name)
    (when min
      (let ((any (%find-any-methodmap-for-class-and-name-range
                  <class> min max)))
        (when any
          (multiple-value-bind (first-idx <module>)
              (unbash any)
            (macrolet
                ((% (from offset)
                   `(iter (for idx ,from (+ first-idx ,offset))
                          (declare (type index-iterator idx))
                          (let ((<methodmap> (bash idx <module> +methodmap+)))
                            (while (and (eql <class> (methodmap-class <methodmap>))
                                        (<= min
                                            (methodmap-name-index <methodmap>)
                                            max)))
                            (funcall fun <methodmap>)))))
              (% from 0)
              (% downfrom -1))))))))

(declaim (inline map-class-methodmaps))
(defun map-class-methods-named (fun <class> name)
  (map-class-methodmaps-named
   (lambda (<methodmap>)
     (map-methodmap-methods fun <methodmap>))
   <class> name))

(deflistify list-class-methods-named map-class-methods-named
  <class>
  name)

(defun list-class-all-methods-named (<class> method-name)
  (labels ((recurse (c)
             (append (list-class-methods-named c method-name)
                     (iter (for super in (list-qclass-superclasses c))
                       (appending (recurse super))))))
    (recurse <class>)))

(let ((unconst-table (make-hash-table)))
  (defun qtype-deconstify (<type>)
    (or (gethash <type> unconst-table)
        (setf (gethash <type> unconst-table)
              (let ((type-name (qtype-name <type>)))
                (if (and (alexandria:starts-with-subseq "const " type-name)
                         (alexandria:ends-with #\& type-name))
                    (or (find-qtype (subseq type-name 6 (1- (length type-name))))
                        <type>)
                    <type>))))))

(let ((qlist-element-table (make-hash-table)))
  (defun qlist-element-type (<type>)
    (multiple-value-bind (result present-p)
        (gethash <type> qlist-element-table)
      (if present-p
          result
          (setf (gethash <type> qlist-element-table)
                (let ((type-name (qtype-name (qtype-deconstify <type>))))
                  (cond ((string= type-name "QStringList")
                         (find-qtype "QString"))
                        ((and (alexandria:starts-with-subseq "QList<" type-name)
                              (alexandria:ends-with #\> type-name))
                         (find-qtype (subseq type-name 6 (1- (length type-name)))))
                        (t nil))))))))

;;;;
;;;; Utilities
;;;;

(defun qapropos (str)
  (setf str (string-upcase str))
  (map-classes (lambda (<class>)
                 (let ((name (qclass-name <class>)))
                   (when (search str (string-upcase name))
                     (format t "Class ~A~%" name)))))
  (map-methods (lambda (<method>)
                 (when (search str (string-upcase (qmethod-name <method>)))
                   (format t "Method ~A~%" (qmethod-fancy-name <method>))))))

(defun find-qclass-ignoring-case (str)
  (block nil
    (map-classes
     (lambda (<class>)
       (when (string-equal (qclass-name <class>) str)
         (return <class>))))))

(defun qmethod-dotted-name (<method>)
  (format nil "~A::~A"
          (qclass-name (qmethod-class <method>))
          (qmethod-name <method>)))

(defun qmethod-fancy-name (<method>)
  (format nil "~A::~A [~D]"
          (qclass-name (qmethod-class <method>))
          (qmethod-name <method>)
          (unbash <method>)))

(defun find-dotted-qmethods (str)
  (let ((result '()))
    (map-methods
     (lambda (method)
       (when (and method (string-equal (qmethod-dotted-name method) str))
         (push method result))))
    result))

(defun describe-methodmap (<methodmap>)
  (format t "~/qt:format-reference/ is a MethodMap~%~%" <methodmap>)
  (format t "    name: ~A~%" (methodmap-name <methodmap>))
  (let ((<class> (methodmap-class <methodmap>)))
    (format t "    for class: ~A (~A)~%" (qclass-name <class>) <class>))
  (format t "~%Methods:~%")
  (describe-methodmap-methods <methodmap>))

(defun describe-qtype (<type>)
  (format t "~/qt:format-reference/ is a type~%~%" <type>)
  (format t "    name: ~A~%" (qtype-name <type>))
  ;; ...
  )

(defun describe-methodmap-methods (<methodmap>)
  (let ((methods (list-methodmap-methods <methodmap>)))
    (cond
     ((null methods)
      ;; fixme?
      )
     ((cdr methods)
      (format t "    ~A~30Tambiguous:~%"
	      (methodmap-name <methodmap>))
      (dolist (method methods)
	(format t "    ~34T~A:~%"
		(qmethod-fancy-name method))))
     (t
      (format t "    ~A~30T~A~%"
	      (methodmap-name <methodmap>)
	      (qmethod-fancy-name (car methods)))))))

(defun describe-qclass-methods (class)
  (dolist (<methodmap> (list-qclass-methodmaps class))
    (describe-methodmap-methods <methodmap>)))

(defun describe-qclass (<class> &optional inherited)
  (format t "~/qt:format-reference/ is a smoke class~%~%" <class>)
  (format t "    name: ~A~%" (qclass-name <class>))
  (format t "    flags: #x~X (~{~A~^, ~})~%"
          (qclass-flags <class>)
          (list-qclass-flags <class>))
  (format t "~%Superclasses:~%")
  (if (list-qclass-superclasses <class>)
      (labels ((recurse (c indent)
                 (dolist (d (list-qclass-superclasses c))
                   (format t "~vT~A~%" indent (qclass-name d))
                   (recurse d (+ indent 4)))))
        (recurse <class> 4))
      (format t "    (none)~%"))
  (format t "~%Methods:~%")
  (describe-qclass-methods <class>)
  (let ((superclasses (list-qclass-superclasses <class>)))
    (when superclasses
      (cond
        (inherited
         (format t "~%Inherited methods:~%")
         (labels ((recurse (c)
                    (dolist (d (list-qclass-superclasses c))
                      (describe-qclass-methods d)
                      (recurse d))))
           (recurse <class>)))
        (t
         (format t "~%Use (QDESCRIBE ~S T) to see inherited methods.~%"
                 (qclass-name <class>))))))
  (describe-qclass-properties <class> inherited))

(defun describe-qmethod (method)
  (format t "~/qt:format-reference/ is a smoke method~%" method)
  (format t "    class: ~A~%" (qmethod-class method))
  (format t "    name: ~A~%" (qmethod-name method))
  (format t "    return type: ~A~%" (qmethod-return-type method))
  (format t "    flags: #x~X (~{~A~^, ~})~%"
          (qmethod-flags method)
          (list-qmethod-flags method))
  (format t "  argument types:~%")
  (if (list-qmethod-argument-types method)
      (dolist (<type> (list-qmethod-argument-types method))
        (format t "    ~A~%" (qtype-name <type>)))
      (format t "    (none)~%")))

(defun class-reference-p (x)
  (multiple-value-bind (id <module> kind) (unbash x)
    (and (eql kind +class+)
	 (<= 0 <module> (1- *n-modules*))
	 (<= 0 id (data-nclasses (data-ref <module>))))))

(defun method-reference-p (x)
  (multiple-value-bind (id <module> kind) (unbash x)
    (and (eql kind +method+)
	 (<= 0 <module> (1- *n-modules*))
	 (<= 0 id (data-nmethods (data-ref <module>))))))

(defun methodmap-reference-p (x)
  (multiple-value-bind (id <module> kind) (unbash x)
    (and (eql kind +methodmap+)
	 (<= 0 <module> (1- *n-modules*))
	 (<= 0 id (data-nmethodmaps (data-ref <module>))))))

(defun type-reference-p (x)
  (multiple-value-bind (id <module> kind) (unbash x)
    (and (eql kind +type+)
	 (<= 0 <module> (1- *n-modules*))
	 (<= 0 id (data-ntypes (data-ref <module>))))))

(defun qdescribe (thing &optional inherited)
  (etypecase thing
    (integer
     (cond
       ((class-reference-p thing) (describe-qclass thing))
       ((method-reference-p thing) (describe-qmethod thing))
       ((type-reference-p thing) (describe-qtype thing))
       ((methodmap-reference-p thing) (describe-methodmap thing))
       (t (format t "Unknown object: ~A~%" thing))))
    (string
     (let ((newlinep nil))
       (let ((class (find-qclass-ignoring-case thing)))
         (when class
           (setf newlinep t)
           (describe-qclass class inherited)))
       (dolist (method (find-dotted-qmethods thing))
         (if newlinep
             (terpri)
             (setf newlinep t))
         (describe-qmethod method))))
    (null-qobject
     (format t "~A is a null pointer~%" thing))
    (qobject
     (describe-qobject thing))))

;;;; Startup stuff

(defvar *cached-objects*)

(defun reload ()
  (setf *n-modules* 0)
  (fill *module-table* nil)
  (fill *module-data-table* nil)
  (setf *cached-objects* (make-hash-table))
  (load-libcommonqt)
  (setf *loaded* t))

(defun ensure-loaded ()
  (unless *loaded*
    (reload)))

(defun ensure-smoke (name)
  (let ((name (string-downcase name)))
    (load-library (format NIL "smoke~a" name))
    (initialize-smoke name)))

(defun initialize-smoke (name)
  (ensure-loaded)
  (let ((name (string-downcase name)))
    (unless (named-module-number name)
      (let ((idx *n-modules*))
        (unless (< idx (length *module-table*))
          (error "Sorry, +module-bits+ exceeded"))
        (let ((init (cffi:foreign-symbol-pointer
                     (format nil "init_~A_Smoke" name))))
          (assert init)
          (cffi:foreign-funcall-pointer init () :void))
        (let ((smoke-struct
                (cffi:mem-ref (cffi:foreign-symbol-pointer
                               (format nil "~A_Smoke" name))
                              :pointer))
              (data (cffi:foreign-alloc '(:struct SmokeData))))
          (setf (svref *module-table* idx) smoke-struct)
          (setf (svref *module-data-table* idx) data)
          (sw_smoke smoke-struct
                    data
                    (cffi:callback deletion-callback)
                    (cffi:callback method-invocation-callback)))
        (incf *n-modules*)
        idx))))

(defun unload ()
  (setf *loaded* nil))

;;; core image workarounds

(defvar *ffi-fasl-pathname* nil)

(defun rebirth ()
  #+ccl
  (load (or *ffi-fasl-pathname*
            (compile-file-pathname
             (merge-pathnames
              "ffi.lisp"
              (asdf:component-pathname (asdf:find-system :qt)))))))
