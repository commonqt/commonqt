;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; Copyright (c) 2007 Ivan Shvedunov. All rights reserved.
;;; Copyright (c) 2007,2010 David Lichteblau. All rights reserved.

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

;; Cache the result of COMPILATION-BODY as long as KEYS still match.
;; This is thread-safe because the cache is replaced atomically.  We will
;; lose cache conses if threads replace them simultaneously.  But that's
;; okay, since correctness is not affected.  Losing some values is easier
;; than having to use locking, and contention is not a case we are
;; optimizing for.
;;
;; zzz extend this to use a vector of multiple cache-conses, using either
;; linear search with round-robin replacement, or using SXHASH-based
;; hashing.  Make the size of that table static, but configurable.
(defmacro with-cache ((&rest keys) &body compilation-body)
  (let ((key-values '())
        (key-tests '()))
    (dolist (key keys)
      (destructuring-bind (value &key (test 'equal)) key
        (push value key-values)
        (push test key-tests)))
    (setf key-values (nreverse key-values))
    (setf key-tests (nreverse key-tests))
    (let* ((keysyms (loop repeat (length keys) collect (gensym)))
           (place (gensym))
           (previous (gensym))
           (check
            (when keysyms
              `((let ((l (cdr ,PREVIOUS)))
                  , (labels ((recurse (vars tests)
                               `(and (,(car tests) (car l) ,(car vars))
                                     ,@ (when (cdr vars)
                                          `((let ((l (cdr l)))
                                              ,(recurse (cdr vars)
                                                        (cdr tests))))))))
                      (recurse keysyms key-tests)))))))
      `(let* ((,PLACE (load-time-value (cons nil nil)))
              (,PREVIOUS (car ,PLACE))
              ,@(mapcar #'list keysyms key-values))
         (cond
           ((and ,PREVIOUS ,@check)
            #+nil(format *trace-output* "cache hit: ~A = ~A => ~A~%"
		    (list ,@keysyms)
		    (cdr ,PREVIOUS)
		    (car ,PREVIOUS))
	    #+nil(force-output *trace-output*)
            (car ,PREVIOUS))
           (t
            (let ((thunk (progn ,@compilation-body)))
	      #+nil(format *trace-output* "cache miss: ~A = ~A => ~A~%"
		      (list ,@keysyms)
		      (cdr ,PREVIOUS)
		      thunk)
	      #+nil(force-output *trace-output*)
              (setf (car ,PLACE) (list thunk ,@keysyms))
              thunk)))))))

(defconstant +cache-pool-size+ 7)

(defmacro cached-values-bind ((&rest vars)
			      value-form
			      &body body)
  (let ((nvars (length vars))
	(key-values '())
        (key-tests '())
        (key-hash-methods '()))
    (iter
      (let ((head (car body)))
	(while (consp head))
	(let ((op (car head)))
	  (while (eq 'provided op))
	  (pop body)
	  (destructuring-bind (value &key (test 'equal) hash) (cdr head)
	    (push value key-values)
	    (push test key-tests)
	    (push hash key-hash-methods)))))
    (setf key-values (nreverse key-values))
    (setf key-tests (nreverse key-tests))
    (setf key-hash-methods (nreverse key-hash-methods))
    (let* ((keysyms (loop repeat (length key-tests) collect (gensym)))
           (places (gensym))
           (hash (gensym))
           (previous (gensym))
           (check
            (when keysyms
              `((let ((l (cdr ,PREVIOUS)))
                  , (labels ((recurse (vars tests)
                               `(and (,(car tests) (car l) ,(car vars))
                                     ,@ (when (cdr vars)
                                          `((let ((l (cdr l)))
                                              ,(recurse (cdr vars)
                                                        (cdr tests))))))))
                      (recurse keysyms key-tests)))))))
      `(multiple-value-bind (,@vars)
	   (let* ((,PLACES (load-time-value (make-array ,+cache-pool-size+
                                                        :initial-element nil)))
		  ,@(mapcar #'list keysyms key-values)
                  (,HASH (mod
                          (logxor ,@(iter (for sym in keysyms)
                                          (for hash-method in key-hash-methods)
                                          (case hash-method
                                            ((nil))
                                            ((t) (collect sym))
                                            (t (collect
                                                   `(,hash-method ,sym))))))
                          ,+cache-pool-size+))
		  (,PREVIOUS (elt ,PLACES ,HASH))
		  (value-array
		   (cond
		     ((and ,PREVIOUS ,@check)
		      #+nil(format *trace-output* "cache hit: ~A = ~A => ~A~%"
				   (list ,@keysyms)
				   (cdr ,PREVIOUS)
				   (car ,PREVIOUS))
		      #+nil(force-output *trace-output*)
		      (car ,PREVIOUS))
		     (t
                      #+nil
                      (format *trace-output*
                              "cache miss on key ~D ~A~%"
                              ,HASH
                              (map 'list (lambda (x) (and x t)) ,PLACES))
                      #+nil (force-output *trace-output*)
		      (let ((value-array (make-array ,nvars)))
			(setf (values
			       ,@(iter (for i from 0 below nvars)
				       (collect `(svref value-array ,i))))
			      ,value-form)
			#+nil(format *trace-output* "cache miss: ~A = ~A => ~A~%"
				     (list ,@keysyms)
				     (cdr ,PREVIOUS)
				     value-array)
			#+nil(force-output *trace-output*)
			(setf (elt ,PLACES ,HASH) (list value-array ,@keysyms))
			value-array)))))
	     (values ,@(iter (for i from 0 below nvars)
			     (collect `(svref value-array ,i)))))
	 ,@body))))
