;;;;
;;;; Evaluate
;;;;   (qt::microbench)
;;;; to run these benchmarks on an otherwise idle computer.  Results are
;;;; written to the REPL, and in a machine readable format also dribbled
;;;; to files.  Files names are, by default, of the form <lisp
;;;; implementation type>.txt.
;;;;
;;;; Notes:
;;;;  1. These are microbenchmarks meant to aid understanding of the
;;;;     implementation.  They do not necessarily reflect overall or
;;;;     real-world performance.
;;;;  2. Since each individual operation is too fast to benchmark, we
;;;;     invoke them a large number of times and compute the average run
;;;;     time afterwards.
;;;;  3. Before running benchmarks, we choose a repetition time depending
;;;;     on how fast (or slow) a simple test case is, so that slow Lisps
;;;;     don't waste endless time running benchmarks.
;;;;  4. Benchmarks are run three times, and only the best run of those
;;;;     three is reported, to account for issues with background activity
;;;;     on the computer ruining the results.
;;;;  5. But you should _still_ run the overall benchmarks several times
;;;;     and see how reproducible the numbers are.
;;;;
;;;; There's no tool to parse the output files and drawn graphs yet, but
;;;; there should be.  (READ-MICROBENCH-RESULTS already fetches the raw
;;;; sexps from each file though, just to check that they are READable).

(in-package :qt)

(named-readtables:in-readtable :qt)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bench
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro measure-dotimes ((var repeat) &body body)
  `(%measure-dotimes (lambda (,var) (declare (ignorable ,var)) ,@body)
		     ,repeat))

(defun %measure-dotimes (fun repeat)
  "Call fun repeatedly without GCing, as often as specified by REPEAT.
   Return the average run time per repetition in microseconds."
  (let ((run0 (get-internal-run-time)))
    (#+ccl ccl::without-gcing
	   #+sbcl sb-sys:without-gcing
	   (dotimes (i repeat)
	     (funcall fun i)))
    (let* ((run1 (get-internal-run-time))
	   (q
	    (float (* (- run1 run0)
		      (/ 1000000000 internal-time-units-per-second)
		      (/ repeat)))))
      (if (< q 10)
	  q
	  (round q)))))

(defparameter *repeat*
  50000)

(defun bench-new-qobject (&optional (repeat *repeat*))
  (let ((objects (make-array repeat)))
    (prog1
	(measure-dotimes (x repeat)
	  (setf (elt objects x) (#_new QObject)))
      (iter (for object in-vector objects)
	    (#_delete object)))))

(defun bench-new-qcolor (&optional (repeat *repeat*))
  (let ((objects (make-array repeat)))
    (prog1
	(measure-dotimes (x repeat)
	  (setf (elt objects x) (#_new QColor)))
      (iter (for object in-vector objects)
	    (#_delete object)))))

(defun bench-new-qcolor/3 (&optional (repeat *repeat*))
  (let ((objects (make-array repeat)))
    (prog1
	(measure-dotimes (x repeat)
	  (setf (elt objects x) (#_new QColor #xca #xfe #xba)))
      (iter (for object in-vector objects)
	    (#_delete object)))))

(defun bench-new-qcolor/4 (&optional (repeat *repeat*))
  (let ((objects (make-array repeat)))
    (prog1
	(measure-dotimes (x repeat)
	  (setf (elt objects x) (#_new QColor #xca #xfe #xba #xbe)))
      (iter (for object in-vector objects)
	    (#_delete object)))))

(defun bench-delete-qobject (&optional (repeat *repeat*))
  (let ((objects (make-array repeat)))
    (dotimes (i repeat)
      (setf (elt objects i)
	    (#_new QObject)))
    (measure-dotimes (i repeat)
      (#_delete (elt objects i)))))

(defun bench-delete-alternating (&optional (repeat *repeat*))
  (let ((objects (make-array repeat)))
    (dotimes (i repeat)
      (setf (elt objects i)
	    (if (evenp i)
		(#_new QObject)
		(#_new QColor))))
    (measure-dotimes (i repeat)
      (#_delete (elt objects i)))))

(defun measure-on-qobjects (fun repeat)
  (let ((objects (make-array repeat)))
    (dotimes (i repeat)
      (setf (elt objects i)
	    (#_new QObject)))
    (prog1
	(measure-dotimes (i repeat)
	  (funcall fun objects i))
      (iter (for object in-vector objects)
	    (#_delete object)))))

(defun bench-call-parent (&optional (repeat *repeat*))
  (measure-on-qobjects (lambda (objects i)
			 (#_parent (elt objects i)))
		       repeat))

(defun bench-call-setparent0 (&optional (repeat *repeat*))
  (let ((x (null-qobject (find-qclass "QObject"))))
    (measure-on-qobjects (lambda (objects i)
			   (#_setParent (elt objects i) x))
			 repeat)))

(defun bench-call-setparent (&optional (repeat *repeat*))
  (let ((others (make-array repeat)))
    (dotimes (i repeat)
      (setf (elt others i)
	    (#_new QObject)))
    (prog1
	(measure-on-qobjects (lambda (objects i)
			       (#_setParent (elt objects i)
					    (elt others i)))
			     repeat)
      (iter (for object in-vector others)
	    (#_delete object)))))

(defun bench-interpret-new-qobject (&optional (repeat *repeat*))
  (let ((objects (make-array repeat)))
    (prog1
	(measure-dotimes (x repeat)
	  (setf (elt objects x) (interpret-new "QObject")))
      (iter (for object in-vector objects)
	    (#_delete object)))))

(defun bench-interpret-new-qcolor (&optional (repeat *repeat*))
  (let ((objects (make-array repeat)))
    (prog1
	(measure-dotimes (x repeat)
	  (setf (elt objects x) (interpret-new "QColor")))
      (iter (for object in-vector objects)
	    (#_delete object)))))

(defun bench-interpret-new-qcolor/3 (&optional (repeat *repeat*))
  (let ((objects (make-array repeat)))
    (prog1
	(measure-dotimes (x repeat)
	  (setf (elt objects x) (interpret-new "QColor" #xca #xfe #xba)))
      (iter (for object in-vector objects)
	    (#_delete object)))))

(defun bench-interpret-new-qcolor/4 (&optional (repeat *repeat*))
  (let ((objects (make-array repeat)))
    (prog1
	(measure-dotimes (x repeat)
	  (setf (elt objects x) (interpret-new "QColor" #xca #xfe #xba #xbe)))
      (iter (for object in-vector objects)
	    (#_delete object)))))

(defun bench-interpret-delete-qobject (&optional (repeat *repeat*))
  (let ((objects (make-array repeat)))
    (dotimes (i repeat)
      (setf (elt objects i)
	    (#_new QObject)))
    (measure-dotimes (i repeat)
      (interpret-delete (elt objects i)))))

(defun bench-interpret-call-parent (&optional (repeat *repeat*))
  (measure-on-qobjects (lambda (objects i)
			 (interpret-call (elt objects i) "parent"))
		       repeat))

(defun bench-interpret-call-setparent0 (&optional (repeat *repeat*))
  (let ((x (null-qobject (find-qclass "QObject"))))
    (measure-on-qobjects (lambda (objects i)
			   (interpret-call (elt objects i) "setParent" x))
			 repeat)))

(defun bench-interpret-call-setparent (&optional (repeat *repeat*))
  (let ((others (make-array repeat)))
    (dotimes (i repeat)
      (setf (elt others i)
	    (#_new QObject)))
    (prog1
	(measure-on-qobjects (lambda (objects i)
			       (interpret-call (elt objects i)
					       "setParent"
					       (elt others i)))
			     repeat)
      (iter (for object in-vector others)
	    (#_delete object)))))

(defun bench/nop (&optional (repeat *repeat*))
  (measure-on-qobjects (lambda (objects i)
			 (declare (ignore objects i)))
		       repeat))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cffi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar <binding>)

(defvar <classfn-qobject>)
(defvar <classfn-qcolor>)

(defvar <marg-qobject>)
(defvar <marg-qobject-dtor>)
(defvar <marg-qcolor>)
(defvar <marg-qcolor/3>)
(defvar <marg-qcolor/4>)
(defvar <marg-qobject-parent>)
(defvar <marg-qobject-set-parent>)

(defmacro %with-stack ((var accessor size) &body body)
  `(cffi:with-foreign-object (,var '(:union StackItem) ,size)
     (macrolet ((,accessor (i slot)
		  `(cffi:foreign-slot-value
		    (cffi:mem-aptr ,',var '(:union StackItem) ,i)
		    '(:union StackItem)
		    ',slot)))
       ,@body)))

(defmacro %call-classfn (fun arg obj stack)
  `(cffi:foreign-funcall-pointer
    ,fun
    ()
    :short ,arg
    :pointer ,obj
    :pointer ,stack
    :void))

(defun bench-new-qobject/cffi (&optional (repeat *repeat*))
  (declare (optimize speed (safety 0) (debug 0)))
  (let ((objects (make-array repeat)))
    (prog1
	(%with-stack (stack item 2)
	  (measure-dotimes (x repeat)
	    (setf (elt objects x)
		  (progn
		    (%call-classfn <classfn-qobject>
				   <marg-qobject>
				   (cffi:null-pointer)
				   stack)
		    (let ((object (item 0 ptr)))
		      (setf (item 1 ptr) <binding>)
		      (%call-classfn <classfn-qobject> 0 object stack)
		      object)))))
      (let ((class (find-qclass "QObject")))
	(iter (for object in-vector objects)
	      (#_delete (%qobject class object)))))))

(defun bench-new-qcolor/cffi (&optional (repeat *repeat*))
  (declare (optimize speed (safety 0) (debug 0)))
  (let ((objects (make-array repeat)))
    (prog1
	(%with-stack (stack item 2)
	  (measure-dotimes (x repeat)
	    (setf (elt objects x)
		  (progn
		    (%call-classfn <classfn-qcolor>
				   <marg-qcolor>
				   (cffi:null-pointer)
				   stack)
		    (let ((object (item 0 ptr)))
		      (setf (item 1 ptr) <binding>)
		      (%call-classfn <classfn-qcolor> 0 object stack)
		      object)))))
      (let ((class (find-qclass "QColor")))
	(iter (for object in-vector objects)
	      (#_delete (%qobject class object)))))))

(defun bench-new-qcolor3/cffi (&optional (repeat *repeat*))
  (declare (optimize speed (safety 0) (debug 0)))
  (let ((objects (make-array repeat)))
    (prog1
	(%with-stack (stack item 4)
	  (measure-dotimes (x repeat)
	    (setf (elt objects x)
		  (progn
		    (setf (item 1 int) 1)
		    (setf (item 2 int) 2)
		    (setf (item 3 int) 3)
		    (%call-classfn <classfn-qcolor>
				   <marg-qcolor/3>
				   (cffi:null-pointer)
				   stack)
		    (let ((object (item 0 ptr)))
		      (setf (item 1 ptr) <binding>)
		      (%call-classfn <classfn-qcolor> 0 object stack)
		      object)))))
      (let ((class (find-qclass "QColor")))
	(iter (for object in-vector objects)
	      (#_delete (%qobject class object)))))))

(defun bench-new-qcolor4/cffi (&optional (repeat *repeat*))
  (declare (optimize speed (safety 0) (debug 0)))
  (let ((objects (make-array repeat)))
    (prog1
	(%with-stack (stack item 5)
	  (measure-dotimes (x repeat)
	    (setf (elt objects x)
		  (progn
		    (setf (item 1 int) 1)
		    (setf (item 2 int) 2)
		    (setf (item 3 int) 3)
		    (setf (item 4 int) 4)
		    (%call-classfn <classfn-qcolor>
				   <marg-qcolor/4>
				   (cffi:null-pointer)
				   stack)
		    (let ((object (item 0 ptr)))
		      (setf (item 1 ptr) <binding>)
		      (%call-classfn <classfn-qcolor> 0 object stack)
		      object)))))
      (let ((class (find-qclass "QColor")))
	(iter (for object in-vector objects)
	      (#_delete (%qobject class object)))))))

(defun bench-delete-qobject/cffi (&optional (repeat *repeat*))
  (let ((objects (make-array repeat)))
    (dotimes (i repeat)
      (setf (elt objects i)
	    (qobject-pointer (#_new QObject))))
    (%with-stack (stack item 1)
	  (measure-dotimes (i repeat)
	    (%call-classfn <classfn-qcolor>
			   <marg-qobject-dtor>
			   (elt objects i)
			   stack)))))

(defun bench-call-parent/cffi (&optional (repeat *repeat*))
  (let ((objects (make-array repeat)))
    (dotimes (i repeat)
      (setf (elt objects i)
	    (qobject-pointer (#_new QObject))))
    (prog1
	(%with-stack (stack item 1)
	  (measure-dotimes (i repeat)
	    (%call-classfn <classfn-qcolor>
			   <marg-qobject-parent>
			   (elt objects i)
			   stack)
	    (item 0 ptr)))
      (let ((class (find-qclass "QObject")))
	(iter (for object in-vector objects)
	      (#_delete (%qobject class object)))))))

(defun bench-call-setparent0/cffi (&optional (repeat *repeat*))
  (let ((objects (make-array repeat)))
    (dotimes (i repeat)
      (setf (elt objects i)
	    (qobject-pointer (#_new QObject))))
    (prog1
	(%with-stack (stack item 2)
	  (measure-dotimes (i repeat)
	    (setf (item 1 ptr) (cffi:null-pointer))
	    (%call-classfn <classfn-qobject>
			   <marg-qobject-set-parent>
			   (elt objects i)
			   stack)
	    (item 0 ptr)))
      (let ((class (find-qclass "QObject")))
	(iter (for object in-vector objects)
	      (#_delete (%qobject class object)))))))

(defun bench-call-setparent/cffi (&optional (repeat *repeat*))
  (let ((objects (make-array repeat))
	(others (make-array repeat)))
    (dotimes (i repeat)
      (setf (elt objects i)
	    (qobject-pointer (#_new QObject)))
      (setf (elt others i)
	    (qobject-pointer (#_new QObject))))
    (prog1
	(%with-stack (stack item 2)
	  (measure-dotimes (i repeat)
	    (setf (item 1 ptr) (elt others i))
	    (%call-classfn <classfn-qobject>
			   <marg-qobject-set-parent>
			   (elt objects i)
			   stack)
	    (item 0 ptr)))
      (let ((class (find-qclass "QObject")))
	(iter (for object in-vector objects)
	      (#_delete (%qobject class object)))
	(iter (for object in-vector others)
	      (#_delete (%qobject class object)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BENCH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init/cffi ()
  (setf <classfn-qobject> (qclass-trampoline-fun (find-qclass "QObject")))
  (setf <classfn-qcolor> (qclass-trampoline-fun (find-qclass "QColor")))
  (setf <marg-qobject> (qmethod-classfn-index
			(find-applicable-method
			 (find-qclass "QObject")
			 "QObject"
			 nil
			 nil)))
  (setf <marg-qobject-dtor> (qmethod-classfn-index
			     (find-applicable-method
			      (find-qclass "QObject")
			      "~QObject"
			      nil
			      nil)))
  (setf <marg-qobject-parent> (qmethod-classfn-index
			       (find-applicable-method
				(find-qclass "QObject")
				"parent"
				nil
				nil)))
  (setf <marg-qobject-set-parent> (qmethod-classfn-index
				   (find-applicable-method
				    (find-qclass "QObject")
				    "setParent"
				    (list (%qobject (find-qclass "QObject")
						    (cffi:null-pointer)))
				    nil)))
  (setf <marg-qcolor> (qmethod-classfn-index
		       (find-applicable-method
			(find-qclass "QColor")
			"QColor"
			nil
			nil)))
  (setf <marg-qcolor/3> (qmethod-classfn-index
			 (find-applicable-method
			  (find-qclass "QColor")
			  "QColor"
			  '(0 0 0)
			  nil)))
  (setf <marg-qcolor/4> (qmethod-classfn-index
			 (find-applicable-method
			  (find-qclass "QColor")
			  "QColor"
			  '(0 0 0 0)
			  nil)))
  (setf <binding> (data-binding (data-ref 0))))

(defun commonqt-directory ()
  (asdf:component-pathname (asdf:find-system :qt)))

(defun dribble-setup-info (s)
  (let ((now (get-universal-time)))
    (format s "(:test-run :date ~A " now)
    (multiple-value-bind (sec min h d month y) (decode-universal-time now)
      (format s ";; ======== ~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D~%"
	      y month d h min sec)))
  (format s ":commonqt ~S~%"
	  (let* ((dir (commonqt-directory))
		 (.git (merge-pathnames ".git/" dir))
		 (ref (with-open-file (s (merge-pathnames "HEAD" .git)
					 :if-does-not-exist nil)
			(and s (subseq (read-line s) 5)))))
	    (if ref
		(with-open-file (s (merge-pathnames ref .git))
		  (subseq (read-line s) 0 8))
		"4.unknown")))
  (format s ":implementation ~S~%"
	  (format nil "~A ~A"
		  (lisp-implementation-type)
		  (lisp-implementation-version)))
  (format s ":machine: ~S~%"
	  (format nil "~A ~A ~A"
		  (machine-type)
		  (machine-version)
		  (machine-instance)))
  (format s ":software ~S~%"
	  (format nil "~A ~A"
		  (software-type)
		  (software-version))))

(defun choose-repeat-count (&optional (fun 'bench-call-parent)
			              (seconds-for-a-test 2))
  ;; run the call-parent microbench for at least a second to estimate
  ;; implementation speed, then choose a good iteration count based on that.
  (let* ((total-time 0)
	 (niterations 0)
	 (1s 1e9)
	 (good-time-for-a-test (* seconds-for-a-test 1s)))
    (iter (until (> total-time 1s))
	  (incf total-time
		(measure-dotimes (dummy 1)
		  (let ((arbitrary-number 1000))
		    (funcall fun arbitrary-number)
		    (incf niterations arbitrary-number)))))
    (ceiling (* niterations (/ good-time-for-a-test total-time)))))

(defun best-of-3-funcall (fun)
  "Call the function three times and return the best result."
  (min (funcall fun)
       (funcall fun)
       (funcall fun)))

(defun microbench
    (&optional (name (lisp-implementation-type)))
  (ensure-smoke :qtcore)
  (ensure-smoke :qtgui)
  (with-open-file (s (make-pathname :name name
				    :type "bench-txt"
				    :defaults (commonqt-directory))
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :append)
    (dribble-setup-info s)
    (let ((*standard-output* (make-broadcast-stream *standard-output* s))
	  (*repeat* (choose-repeat-count)))
      (format s ":repeat-count ~D~%" *repeat*)
      (init/cffi)
      (format s ":results (~%")
      (dolist (fun '(bench/nop
		     bench-new-qobject
		     bench-delete-qobject
		     bench-new-qcolor
		     bench-new-qcolor/3
		     bench-new-qcolor/4
		     bench-call-parent
		     bench-call-setparent0
		     bench-call-setparent))
	(format t "(~A ~30T~7D)~%" fun (best-of-3-funcall fun)))
      ;; give the interpreted functions their own repeat count to avoid
      ;; long delays:
      (let ((*repeat* (choose-repeat-count 'bench-interpret-call-parent)))
	(dolist (fun '(bench-interpret-new-qobject
		       bench-interpret-delete-qobject
		       bench-interpret-new-qcolor
		       bench-interpret-new-qcolor/3
		       bench-interpret-new-qcolor/4
		       bench-interpret-call-parent
		       bench-interpret-call-setparent0
		       bench-interpret-call-setparent))
	  (format t "(~A ~30T~6D)~%" fun (best-of-3-funcall fun))))
      ;;
      ;; The /CFFI tests do not benchmark CommonQt as such; they show
      ;; how fast we "would" be able to run if we had "optimal"
      ;; performance while still using kdebindings.  The use cffi to
      ;; call smoke as efficiently as possible, assuming perfect type
      ;; information, no runtime dispatch, etc.
      ;;
      (format t ";; the following numbers are for comparison only:~%")
      (let ((*repeat* (choose-repeat-count
		       'bench-new-qcolor/cffi
		       ;; hmm, need to force a higher repeat count...:
		       5)))
	(dolist (fun '(bench-new-qobject/cffi
		       bench-delete-qobject/cffi
		       bench-new-qcolor/cffi
		       bench-new-qcolor3/cffi
		       bench-new-qcolor4/cffi
		       bench-call-parent/cffi
		       bench-call-setparent0/cffi
		       bench-call-setparent/cffi))
	  (format t "(~A ~30T~6D)~%" fun (best-of-3-funcall fun)))))
    (format s "))~%")))

(defun read-microbench-results (&optional (name (lisp-implementation-type)))
  (with-open-file (s (make-pathname :name name
				    :type "bench-txt"
				    :defaults (commonqt-directory)))
    (iter (for form = (read s nil))
	  (while form)
	  (collect form))))
