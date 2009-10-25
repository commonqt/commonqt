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

;;;;
;;;; Module
;;;;

(defconstant +module-bits+ 4)

(defvar *n-modules* 0)

(defvar *module-table*
  (make-array (ash 1 +module-bits+) :initial-element nil))

(defvar *module-data-table*
  (make-array (ash 1 +module-bits+) :initial-element nil))

#-debug (declaim (inline module-ref))
(defun module-ref (i) (svref *module-table* i))

#-debug (declaim (inline data-ref))
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

(defconstant +kind-bits+ 2)

(defconstant +class+ 0)
(defconstant +method+ 1)
(defconstant +methodmap+ 2)
(defconstant +type+ 3)

(defun module-number (smoke)
  (position smoke *module-table* :test #'cffi:pointer-eq))

(defun named-module-number (name)
  (position name
            *module-data-table*
            :key (lambda (data)
                   (and data (data-name data)))
            :test #'string=))

#-debug (declaim (inline bash))
(defun bash (idx module-number kind)
  (logior kind
          (ash (logior module-number
                       (ash idx +module-bits+))
               +kind-bits+)))

#-debug (declaim (inline ldb-module))
(defun ldb-module (x)
  (ldb (byte +module-bits+ +kind-bits+) x))

#-debug (declaim (inline ldb-kind))
(defun ldb-kind (x)
  (ldb (byte +kind-bits+ 0) x))

#-debug (declaim (inline unbash))
(defun unbash (x)
  (values (ldb (byte 16 (+ +module-bits+ +kind-bits+)) x)
          (ldb-module x)
          (ldb (byte +kind-bits+ 0) x)))

#-debug (declaim (inline unbash*))
(defun unbash* (x expected-kind)
  (multiple-value-bind (idx <module> kind)
      (unbash x)
    #+debug (assert (eql kind expected-kind))
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
        (map-classes-in-module fun <module> allow-external)))

(defun map-classes-in-module (fun <module> &optional allow-external)
  (let ((n (data-nclasses (data-ref <module>))))
    (iter (for i from 1 below n)
	  (let ((<class> (bash i <module> +class+)))
	    (unless (or allow-external (qclass-external-p <class>))
	      (funcall fun <class>))))))

(defun qclass-struct (<class>)
  (multiple-value-bind (idx <module>)
      (unbash* <class> +class+)
    #+debug (assert (<= 0 idx (data-nclasses (data-ref <module>))))
    (cffi:mem-aref (data-classes (data-ref <module>))
                   '|struct Class|
                   idx)))

(defun qclass-name (<class>)
  (cffi:foreign-slot-value (qclass-struct <class>) '|struct Class| 'classname))

(defun qclass-external-p (<class>)
  (plusp
   (cffi:foreign-slot-value (qclass-struct <class>) '|struct Class| 'external)))

(defun resolve-external-qclass (<class>)
  (if (qclass-external-p <class>)
      (find-qclass (qclass-name <class>))
      <class>))

(defun find-qclass (name)
  (etypecase name
    (integer
     (assert (eql (ldb-kind name) +class+))
     name)
    (string
      (cffi:with-foreign-object (&smoke :pointer)
        (cffi:with-foreign-object (&index :short)
          (sw_find_class name &smoke &index)
	  (let ((smoke (cffi:mem-ref &smoke :pointer)))
	    (unless (cffi:null-pointer-p smoke)
	      (bash (cffi:mem-ref &index :short)
		    (module-number smoke)
		    +class+))))))))

(defmacro deflistify (list-name map-name &rest args)
  `(defun ,list-name (,@args)
     (iter
      (repeat 1)
      (,map-name (lambda (x) (collect x)) ,@args))))

(deflistify list-qclass-superclasses map-qclass-superclasses
  <class>)

(defun format-reference (stream arg foo bar)
  (declare (ignore foo bar))
  (multiple-value-bind (id <module> kind)
      (unbash arg)
    (format stream "~D <~D,~D,~D>" arg id <module> kind)))

(defun map-qclass-superclasses (fun <class>)
  (let* ((<module> (ldb-module <class>))
         (parents (cffi:foreign-slot-value (qclass-struct <class>)
                                           '|struct Class|
                                           'parents))
         (inheritancelist (data-inheritancelist (data-ref <module>))))
    (iter (for i from parents)
          (let ((classid (cffi:mem-aref inheritancelist :short i)))
            (while (plusp classid))
            (funcall fun (resolve-external-qclass
                          (bash classid <module> +class+)))))))

(defun qclass-flags (<class>)
  (cffi:foreign-slot-value (qclass-struct <class>) '|struct Class| 'flags))

(macrolet ((deftest (name mask)
             `(defun ,name (<class>)
                (logtest ,mask (qclass-flags <class>)))))
  (deftest qclass-constructor-p #x01)
  (deftest qclass-deepcopy-p #x02)
  (deftest qclass-virtual-p #x04)
  (deftest qclass-undefined-p #x08))

(defun list-qclass-flags (<class>)
  (let ((x '()))
    (when (qclass-constructor-p <class>) (push :constructor x))
    (when (qclass-deepcopy-p <class>) (push :deepcopy x))
    (when (qclass-virtual-p <class>) (push :virtual x))
    (when (qclass-undefined-p <class>) (push :undefined x))
    x))

(defun qclass-trampoline-fun (<class>)
  (cffi:foreign-slot-value (qclass-struct <class>) '|struct Class| 'classfn))

(defun qclass-enum-fun (<class>)
  (cffi:foreign-slot-value (qclass-struct <class>) '|struct Class| 'enumfn))

(defun %find-any-methodmap-for-class (<class>)
  ;; Note: The way the comparison is currently written depends on <class>
  ;; order being the same as index order within the same module.
  (let* ((<module> (ldb-module <class>))
         (from 1)
         (to (data-nmethodmaps (data-ref <module>))))
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

(defun map-class-methodmaps (fun <class>)
  (let ((any (%find-any-methodmap-for-class <class>)))
    (when any
      (multiple-value-bind (first-idx <module>)
			   (unbash any)
	(macrolet 
	    ((% (from offset)
	       `(iter (for idx ,from (+ first-idx ,offset))
		      (let ((<methodmap> (bash idx <module> +methodmap+)))
			(while (eql <class> (methodmap-class <methodmap>)))
			(funcall fun <methodmap>)))))
	  (% from 0)
	  (% downfrom -1))))))


;;;;
;;;; MethodMap
;;;;

(defun methodmap-struct (<methodmap>)
  (multiple-value-bind (idx <module>)
      (unbash* <methodmap> +methodmap+)
    #+debug (assert (<= 0 idx (data-nmethodmaps (data-ref <module>))))
    (cffi:mem-aref (data-methodmaps (data-ref <module>))
                   '|struct MethodMap|
                   idx)))

(defun methodmap-class (<methodmap>)
  (bash (cffi:foreign-slot-value (methodmap-struct <methodmap>)
                                 '|struct MethodMap|
                                 'classid)
        (ldb-module <methodmap>)
        +class+))

(deflistify list-methodmap-methods map-methodmap-methods
  <methodmap>)

(defun map-methodmap-methods (fun <methodmap>)
  (let ((<module> (ldb-module <methodmap>))
        (methodid (cffi:foreign-slot-value (methodmap-struct <methodmap>)
                                           '|struct MethodMap|
                                           'methodid)))
    (if (plusp methodid)
        (funcall fun (bash methodid <module> +method+))
        (let ((ambiguous-methods
               (data-ambiguousMethodList (data-ref <module>))))
          (iter (for i from (- methodid))
                (let ((id (cffi:mem-aref ambiguous-methods :short i)))
                  (while (plusp id))
                  (funcall fun (bash id <module> +method+))))))))

(defun name-ref (<module> idx)
  (cffi:mem-aref (data-methodnames (data-ref <module>))
                 :string
                 idx))

(defun methodmap-name (<methodmap>)
  (name-ref (ldb-module <methodmap>)
            (cffi:foreign-slot-value (methodmap-struct <methodmap>)
                                     '|struct MethodMap|
                                     'name)))

(defun find-methodmap (<class> name)
  (multiple-value-bind (classid <module>)
      (unbash* <class> +class+)
    (let ((smoke (module-ref <module>)))
      (bash (sw_id_method smoke classid (%find-name smoke name))
	    <module>
	    +methodmap+))))


;;;;
;;;; Methods
;;;;

(defun map-methods (fun)
  (iter (for <module> below *n-modules*)
        (map-methods-in-module fun <module>)))

(defun map-methods-in-module (fun <module>)
  (let ((n (data-nmethods (data-ref <module>))))
    (iter (for i from 0 below n)
          (funcall fun (bash i <module> +method+)))))

(defun qmethod-struct (<method>)
  (multiple-value-bind (idx <module>)
      (unbash* <method> +method+)
    #+debug (assert (<= 0 idx (data-nmethods (data-ref <module>))))
    (cffi:mem-aref (data-methods (data-ref <module>))
                   '|struct Method|
                   idx)))

(defun qmethod-class (<method>)
  (bash (cffi:foreign-slot-value (qmethod-struct <method>)
                                 '|struct Method|
                                 'classid)
        (ldb-module <method>)
        +class+))

(defun qmethod-name (<method>)
  (name-ref (nth-value 1 (unbash <method>))
            (cffi:foreign-slot-value (qmethod-struct <method>)
                                     '|struct Method|
                                     'name)))

(defun qmethod-flags (<method>)
  (cffi:foreign-slot-value (qmethod-struct <method>) '|struct Method| 'flags))

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
  (deftest qmethod-protected-p #x80))

(defun qmethod-return-type (<method>)
  (bash (cffi:foreign-slot-value (qmethod-struct <method>)
				 '|struct Method|
				 'ret)
	(ldb-module <method>)
	+type+))

(defun map-qmethod-argument-types (fun <method>)
  (let* ((<module> (ldb-module <method>))
         (argumentlist (data-argumentlist (data-ref <module>))))
    (cffi:with-foreign-slots
        ((args numargs) (qmethod-struct <method>) |struct Method|)
      (iter (for i from args)
            (repeat numargs)
            (funcall fun
                     (bash (cffi:mem-aref argumentlist :short i)
                           <module>
                           +type+))))))

(deflistify list-qmethod-argument-types map-qmethod-argument-types
  <method>)

(defun qmethod-arg-for-classfn (<method>)
  (cffi:foreign-slot-value (qmethod-struct <method>)
                           '|struct Method|
                           'methodForClassFun))

;;;;
;;;; Type
;;;;

;; fixme: zerop flags; zerop classid

(defun qtype-void-p (<type>)
  (zerop (unbash <type>)))

(defun map-types (fun)
  (map-types-in-module fun 0))

(defun map-types-in-module (fun <module> &optional allow-external)
  (let ((n (data-ntypes (data-ref <module>))))
    (iter (for i from 1 below n)
	  (funcall fun (bash i <module> +type+)))))

(defun qtype-struct (<type>)
  (multiple-value-bind (idx <module>)
      (unbash* <type> +type+)
    #+debug (assert (<= 0 idx (data-ntypes (data-ref <module>))))
    (cffi:mem-aref (data-types (data-ref <module>))
                   '|struct Type|
                   idx)))

(defun qtype-class (<type>)
  (resolve-external-qclass
   (bash (cffi:foreign-slot-value (qtype-struct <type>)
				  '|struct Type|
				  'classid)
	 (ldb-module <type>)
	 +class+)))

(defun qtype-name (<type>)
  (cffi:foreign-slot-value (qtype-struct <type>) '|struct Type| 'name))

(defun qtype-interned-name (<type>)
  (intern (qtype-name <type>) :keyword))

(defun qtype-flags (<type>)
  (cffi:foreign-slot-value (qtype-struct <type>) '|struct Type| 'flags))

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

(defun find-qtype (name &optional (<module> 0))
  (let ((index (sw_id_type (elt *module-table* <module>) name)))
    (and (plusp index) (bash index <module> +type+))))


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

(deflistify list-qclass-methodmaps map-class-methodmaps
  <class>)

(defun describe-methodmap (<methodmap>)
  (format t "~/qt:format-reference/ is a MethodMap~%~%" <methodmap>)
  (format t "    name: ~A~%" (methodmap-name <methodmap>))
  (let ((<class> (methodmap-class <methodmap>)))
    (format t "    for class: ~A (~A)~%" (qclass-name <class>) <class>))
  (format t "~%Methods:~%")
  (describe-methodmap-methods <methodmap>))

(defun describe-qtype (<type>)
  (format t "~/qt:format-reference/ is a type~%~%" <type>)
  (format t "    name: ~A~%" (qtype-name <methodmap>))
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
  (format t "    flags:~{ ~A~^,~}~%" (qmethod-flags method))
  (format t "  argument types:~%")
  (if (list-qmethod-argument-types method)
      (dolist (type (list-qmethod-argument-types method))
        (format t "    ~A~%" type))
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
(defvar *keep-alive*)
(defvar *qobject-metaobject* nil)
(defvar *smoke-instance-list* (list nil nil))
(defvar *smoke-instances-by-pointer*)

(defun reload ()
  (setf *n-modules* 0)
  (fill *module-table* nil)
  (fill *module-data-table* nil)
  (setf *cached-objects* (tg:make-weak-hash-table :weakness :value))
  (setf *keep-alive* (make-hash-table))
  (setf *qobject-metaobject* nil)
  (load-libcommonqt))

(defun ensure-loaded ()
  (unless *loaded*
    (reload)))

(defun ensure-smoke (&optional (name :qt))
  (ensure-loaded)
  (let ((name (string-downcase name)))
    (unless (named-module-number name)
      (let ((idx *n-modules*))
        #+debug (assert (< idx (length *module-table*)))
        (cffi:load-foreign-library (format nil "libsmoke~A.so" name))
        (let ((init (cffi:foreign-symbol-pointer
                     (format nil "_Z~Dinit_~A_Smokev"
                             (+ 11 (length name))
                             name))))
          (assert init)
          (cffi:foreign-funcall-pointer init () :void))
        (let ((smoke-struct
               (cffi:mem-ref (cffi:foreign-symbol-pointer
                              (format nil "~A_Smoke" name))
                             :pointer))
              (data (cffi:foreign-alloc '|struct SmokeData|)))
          (setf (svref *module-table* idx) smoke-struct)
          (setf (svref *module-data-table* idx) data)
          (sw_smoke smoke-struct
                    data
                    (cffi:callback deletion-callback)
                    (cffi:callback method-invocation-callback)
                    (cffi:callback child-callback)))
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
