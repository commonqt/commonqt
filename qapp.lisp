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
(named-readtables:in-readtable :qt)

;;; Create a QApplication from command line arguments.
;;;
;;; Usually, programs just pass argc and argv from their `main' function here.
;;; We take a Lisp list and set up a fresh argv array.
;;;
;;; First return value is the QApplication.
;;;
;;; Second return value is an update array of arguments.
;;; For example, "-display" "foo" will have been removed afterwards.

(defvar *qapplication* nil)
(defvar *qapplication-create-hooks* ())

(defmacro with-main-window ((window form) &body body)
  `(progn
     (make-qapplication)
     (with-objects ((,window ,form))
       ,@body
       (#_show ,window)
       (#_exec *qapplication*))))

(defun make-qapplication (&rest args)
  (cond (*qapplication*)
        (t
         (ensure-smoke :qtcore)
         (ensure-smoke :qtgui)
         (let ((instance (#_QCoreApplication::instance)))
           (setf *qapplication*
                 (if (null-qobject-p instance)
                     (%make-qapplication (cons "argv0dummy" args))
                     instance)))
         (dolist (hook *qapplication-create-hooks*)
           (funcall hook *qapplication*))
         *qapplication*)))

(defun %make-qapplication (args &optional (guip t))
  (unless args
    (error "argv[0] not specified"))
  (mapc (lambda (arg) (check-type arg string)) args)
  (let* ((args (coerce args 'simple-vector))
         (argv
          ;; Memory leak: This array must not be freed earlier than the
          ;; QApplication.  Let's just leak it.
          (string-vector-to-char** args))
         (&argc
          ;; Apparently this, too, needs to have extent for more than the ctor.
          (cffi:foreign-alloc :int)))
    (setf (cffi:mem-aref &argc :int) (length args))
    (let* ((qapplication
            (if guip
                (#_new QApplication &argc argv)
                (#_new QCoreApplication &argc argv)))
           (updated-argc (cffi:mem-aref &argc :int)))
      (values qapplication
              (char**-to-string-vector argv updated-argc nil)))))

(defun describe-metaobject-methods (mo)
  (let* ((offset (#_methodOffset mo)))
    (format t "Metaobject ~A~%" mo)
    (format t "  superClass:~%")
    (format t "~10T~A~%" (#_superClass mo))
    (format t "  inherited methods:~%")
    (dotimes (i offset)
      (format t "~10T~A~%" (#_signature (#_method mo i))))
    (format t "  direct methods:~%")
    (loop for i from offset below (#_methodCount mo) do
      (format t "~10T~A~%" (#_signature (#_method mo i))))))

(defun describe-metamethods (object)
  (format t "Metaobject for ~A:~%" object)
  (describe-metaobject-methods (qobject-metaobject object)))

(defun windows-version ()
  (let ((v (sw_windows_version)))
    (if (minusp v) nil v)))

(defvar +xp+ #x30)
(defvar +2003+ #x40)
(defvar +vista+ #x80)

(defun set-nice-theme ()
  ;; This function isn't called by CommonQt automatically, but user
  ;; code can use it if desired.
  ;;
  ;; The native look on Vista is great, but XP's widgets look antiquated.
  ;; Let's use Plastique on XP instead.
  ;;
  ;; On non-Windows, we don't have this problem, so do nothing.
  ;;
  (ensure-smoke :qtcore)
  (ensure-smoke :qtgui)
  (let ((v (windows-version)))
    (when (and v (< v +vista+))
      (#_QApplication::setStyle "Plastique"))))

(defmacro with-int& ((var value) &body body)
  `(invoke-with-int& (lambda (,var) ,@body) ,value))

(defun invoke-with-int& (fun value)
  (cffi:with-foreign-object (reference :int)
    (setf (cffi:mem-aref reference :int) value)
    (funcall fun reference)
    (cffi:mem-aref reference :int)))

(defmacro with-&bool ((var value) &body body)
  `(invoke-with-&bool (lambda (,var) ,@body) ,value))

(defun invoke-with-&bool (fun value)
  (cffi:with-foreign-object (reference :int)
    (setf (cffi:mem-aref reference :int) (if value 1 0))
    (funcall fun reference)
    (logtest 1 (cffi:mem-aref reference :int))))

(defmacro with-char** ((var string-list) &body body)
  `(invoke-with-char** (lambda (,var) ,@body) ,string-list))

(defun invoke-with-char** (fun data)
  (loop
     for item across data
     do (check-type item string))
  (cffi:with-foreign-object (argv :pointer (length data))
    (string-vector-to-char**! argv data)
    (funcall fun argv)
    (char**-to-string-vector! data argv (length data) t)))
