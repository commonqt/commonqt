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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-smoke-lambda/quotes (stream char n)
    (declare (ignore n))
    (check-type char (eql #\"))
    (let* ((method-name (with-output-to-string (s)
                          (loop
                             for c = (read-char stream)
                             until (eql c #\")
                             do (write-char c s)))))
      `(lambda (instance &rest args)
         (apply #'call instance ,method-name args)))))

(defvar *case-preserving-readtable*
  (let ((table (copy-readtable nil)))
    (setf (readtable-case table) :preserve)
    table))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-smoke-lambda/quoteless (stream char n)
    (declare (ignore n))
    (check-type char (eql #\_))
    (flet ((next ()
             (symbol-name
              (let ((*readtable* *case-preserving-readtable*))
                (read stream t nil t)))))
      (let ((method-name (next)))
        (if (equal method-name "new")   ;it's magic!
            (let ((class-name (next)))
              `(lambda (&rest args)
                 (apply #'new ,class-name args)))
            `(lambda (instance &rest args)
               (apply #'call instance ,method-name args)))))))

(named-readtables:defreadtable :qt
    (:merge :standard)
  (:dispatch-macro-char #\# #\" 'read-smoke-lambda/quotes)
  (:dispatch-macro-char #\# #\_ 'read-smoke-lambda/quoteless))
