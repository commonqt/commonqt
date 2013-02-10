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
  (let ((table (copy-readtable)))
    (setf (readtable-case table) :preserve)
    table))

;;; To support [#_new QLabel] or <#_new QLabel> read not just until
;;; the #\) character but until the character with a macro-character
;;; function equal to #\) macro-character function. The idea is that
;;; ] will usually be defined as (set-syntax-from-char #\] #\)) or
;;; (set-macro-character #\} (get-macro-character #\)))
(defun read-list-until (char stream &optional (recursive-p t))
  (let ((char-macro (get-macro-character char)))
    (assert char-macro)
    (loop with read
          for next-char = (peek-char t stream t nil recursive-p)
          until (char= char next-char)
          if
          (let ((macro (get-macro-character next-char)))
            ;; Need to go through this in order to be able to ignore
            ;; comments and feature expression at the tail of a list.
            (cond ((not macro)
                   (setf read (read stream t nil recursive-p))
                   t)
                  ((eq char-macro macro)
                   (loop-finish))
                  (t
                   (setf read
                         (multiple-value-list
                          (funcall macro stream
                                   (read-char stream t nil recursive-p))))
                   (when read
                     (setf read (car read))
                     t))))
          collect read)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-smoke-lambda (stream char n)
    (declare (ignore char n))
    (flet ((expand-to (&rest args)
             `(lambda ()
                (,@args ,@(read-list-until #\) stream)))))
      (let ((method-name
              (coerce (iter
                        (let ((char (peek-char nil stream t nil t)))
                          (if (or (char= char #\_)
                                  (char= char #\:)
                                  (char= char #\~)
                                  (char<= #\A char #\Z)
                                  (char<= #\a char #\z)
                                  (char<= #\0 char #\9)
                                  (char<= #\< char #\>)) ; < = >
                              (collect (read-char stream t nil t))
                              (finish))))
                      'string)))
        (if (ppcre:scan "^[<=>]+$" method-name)
            (setf method-name
                  (concatenate 'string "operator" method-name)))
        (cond
          ((equal method-name "delete")
           (expand-to 'optimized-delete))
          ((equal method-name "new")
           (let ((class-name
                   (symbol-name
                    (let ((*readtable* *case-preserving-readtable*))
                      (read stream t nil t)))))
             (expand-to 'optimized-new class-name)))
          ((let ((position (search "::" method-name)))
             (when position
               (expand-to
                'optimized-call t
                (subseq method-name 0 position)
                (subseq method-name (+ position 2))))))
          (t
           (let ((args (read-list-until #\) stream)))
            `(lambda ()
               ,(cond ((equal method-name "")
                       `(error "Invalid #_ syntax: \"#_\"" ,method-name))
                      ((zerop (length args))
                       `(error "Invalid number of arguments for #_~a: 0."
                               ,method-name))
                      (t
                       `(optimized-call t ,(car args) ,method-name
                                        ,@(cdr args))))))))))))

(named-readtables:defreadtable :qt
    (:merge :standard)
  (:dispatch-macro-char #\# #\_ 'read-smoke-lambda))
