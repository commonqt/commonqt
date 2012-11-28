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
              `((let ((l (cdr ,previous)))
                  , (labels ((recurse (vars tests)
                               `(and (,(car tests) (car l) ,(car vars))
                                     ,@ (when (cdr vars)
                                          `((let ((l (cdr l)))
                                              ,(recurse (cdr vars)
                                                        (cdr tests))))))))
                      (recurse keysyms key-tests)))))))
      `(let* ((,place (load-time-value (cons nil nil)))
              (,previous (car ,place))
              ,@(mapcar #'list keysyms key-values))
         (cond
           ((and ,previous ,@check)
            #+nil(format *trace-output* "cache hit: ~a = ~a => ~a~%"
		    (list ,@keysyms)
		    (cdr ,previous)
		    (car ,previous))
	    #+nil(force-output *trace-output*)
            (car ,previous))
           (t
            (let ((thunk (progn ,@compilation-body)))
	      #+nil(format *trace-output* "cache miss: ~a = ~a => ~a~%"
		      (list ,@keysyms)
		      (cdr ,previous)
		      thunk)
	      #+nil(force-output *trace-output*)
              (setf (car ,place) (list thunk ,@keysyms))
              thunk)))))))

(defconstant +cache-pool-size+ 7)

(defun parse-cache-values (forms)
  (let (values tests hash-methods)
    (dolist (form forms)
      (destructuring-bind (value &key (test 'equal) hash) form
        (push value values)
        (push test tests)
        (push hash hash-methods)))
    (values (nreverse values)
            (nreverse tests)
            (nreverse hash-methods))))

(defun cache-test (previous key-syms key-tests)
  `(let ((l (cdr ,previous)))
     ,(labels ((recurse (vars tests)
                        `(and (,(car tests) (car l) ,(car vars))
                              ,@ (when (cdr vars)
                                   `((let ((l (cdr l)))
                                       ,(recurse (cdr vars)
                                                 (cdr tests))))))))
              (recurse key-syms key-tests))))

(defun cache-hash (key-syms key-hash-methods)
  `(mod
    (logxor ,@(loop for sym in key-syms
                    for hash-method in key-hash-methods
                    when hash-method
                    collect (if (eql hash-method 't)
                                sym
                                `(,hash-method ,sym))))
    +cache-pool-size+))

(defmacro cached-values-bind (vars
			      value-form
                              cache-values
			      &body body)
  "Bind variables in VARS to values of VALUE-FORM
caching the result based on the values specified in CACHE-VALUES
in the form ((value &key (test 'equal) hash)*).
HASH may be:
T, in which case the value of a variable must be an integer,
function of one argument producing an integer, e.g. SXHASH.
NIL, meaning the value will not be used in hashing."
  (multiple-value-bind (key-values key-tests key-hash-methods)
      (parse-cache-values cache-values)
    (let* ((key-syms (loop repeat (length key-tests) collect (gensym)))
           (places (gensym "PLACES-"))
           (hash (gensym "HASH-"))
           (previous (gensym "PREVIOUS-"))
           (check (when key-syms (cache-test previous key-syms key-tests))))
      `(multiple-value-bind (,@vars)
           (let* ((,places (load-time-value (make-array +cache-pool-size+
                                                        :initial-element nil)))
                  ,@(mapcar #'list key-syms key-values)
                  (,hash ,(cache-hash key-syms key-hash-methods))
                  (,previous (aref ,places ,hash))
                  (values
                   (cond
                     ((and ,previous ,check)
                      (car ,previous))
                     (t
                      (let ((values ,(if (= (length vars) 1)
                                         value-form
                                         `(multiple-value-list ,value-form))))
                        (setf (aref ,places ,hash) (list values ,@key-syms))
                        values)))))
             ,(if (= (length vars) 1)
                  'values
                  '(values-list values)))
         ,@body))))
