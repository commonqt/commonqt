;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; http://doc.trolltech.com/4.3/tutorial-t1.html

(defpackage :qt-tutorial-1
  (:use :cl :qt)
  (:export #:main))

(in-package :qt-tutorial-1)
(named-readtables:in-readtable :qt)

(defvar *qapp*)

(defun main ()
  (setf *qapp* (make-qapplication))

  (let ((hello (#_new QPushButton "Hello World!")))
    (#_resize hello 100 30)
    (#_show hello)

    (unwind-protect
         (#_exec *qapp*)
      (#_hide hello))))
