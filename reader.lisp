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

(defvar *case-preserving-readtable*
  (let ((table (copy-readtable nil)))
    (setf (readtable-case table) :preserve)
    table))


;;; CALL

(defvar *call-macros* (make-hash-table :test 'equal))

(defun call-macro-expander (whole env)
  (declare (ignore env))
  (destructuring-bind (sym instance &rest args) whole
    (let ((method-name (symbol-value sym)))
      `(optimized-call t ,instance ,method-name ,@args))))

(defun ensure-call-macro (name)
  (or (gethash name *call-macros*)
      (setf (gethash name *call-macros*)
            (let ((sym (gensym name)))
              (setf (symbol-value sym) name)
              (setf (macro-function sym) #'call-macro-expander)
              sym))))


;;; STATIC CALL

(defvar *static-call-macros* (make-hash-table :test 'equal))

(defun static-call-macro-expander (whole env)
  (declare (ignore env))
  (destructuring-bind (sym &rest args) whole
    (destructuring-bind (class-name method-name)
        (symbol-value sym)
      `(optimized-call t ,class-name ,method-name ,@args))))

(defun ensure-static-call-macro (class-name method-name)
  (let ((key (list class-name method-name)))
    (or (gethash key *static-call-macros*)
        (setf (gethash key *static-call-macros*)
              (let ((sym (gensym
                          (concatenate 'string class-name "::" method-name))))
                (setf (symbol-value sym) key)
                (setf (macro-function sym) #'static-call-macro-expander)
                sym)))))

;;; NEW

(defvar *new-macros* (make-hash-table :test 'equal))

(defun new-macro-expander (whole env)
  (declare (ignore env))
  (destructuring-bind (sym &rest args) whole
    (let ((class-name (symbol-value sym)))
      `(optimized-new ,class-name ,@args))))

(defun ensure-new-macro (name)
  (or (gethash name *new-macros*)
      (setf (gethash name *new-macros*)
            (let ((sym (gensym name)))
              (setf (symbol-value sym) name)
              (setf (macro-function sym) #'new-macro-expander)
              sym))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-smoke-lambda (stream char n &aux it)
    (declare (ignore n))
    (check-type char (eql #\_))
    (let ((method-name
           (coerce (iter
                     (let* ((char (peek-char nil stream))
                            (code (char-code char)))
                       (if (or (eql char #\_)
                               (eql char #\:)
                               (<= 65 code 90)
                               (<= 97 code 122)
                               (<= 48 code 57))
                           (collect (read-char stream))
                           (finish))))
                   'string)))
      (cond
        ((equal method-name "delete")
         'optimized-delete)
        ((equal method-name "new")
         (let ((class-name
                (symbol-name
                 (let ((*readtable* *case-preserving-readtable*))
                   (read stream t nil t)))))
           (ensure-new-macro class-name)))
        ((setf it (search "::" method-name))
         (let ((class-name (subseq method-name 0 it))
               (method-name (subseq method-name (+ it 2))))
           (ensure-static-call-macro class-name method-name)))
        (t
         (ensure-call-macro method-name))))))

(named-readtables:defreadtable :qt
    (:merge :standard)
  (:dispatch-macro-char #\# #\_ 'read-smoke-lambda))
