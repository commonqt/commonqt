(in-package :qt)
#+sbcl (declaim (optimize (debug 2)))
(named-readtables:in-readtable :qt)

(defun type-for-qvariant (x)
  (etypecase x
    (integer :int)))

(defun qvariant (x &optional (type (type-for-qvariant x)))
  (ecase type
    (:int (#_new QVariant x))))

(defmacro dcase (form &body clauses)
  (let ((value (gensym)))
    `(let ((,VALUE ,form))
       (cond
	 ,@(iter (for (key . body) in clauses)
		 (collect
		     (if (eq key t)
			 `(t ,@body)
			 `((eql ,key ,VALUE) ,@body))))))))

(defun unvariant (x)
  (dcase (#_type x)
    ((#_QVariant::Int) (#_toInt x))))
