;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; http://doc.trolltech.com/4.3/tutorial-t3.html

(defpackage :qt-tutorial-4
  (:use :cl :qt)
  (:export #:main))

(in-package :qt-tutorial-4)
(named-readtables:in-readtable :qt)

(defvar *qapp*)

(defclass my-widget ()
    ()
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((instance my-widget) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (#_setFixedSize instance 200 120)
  (let ((quit (#_new QPushButton "Quit" instance)))
    (#_setGeometry quit 62 40 75 30)
    (#_setFont quit (#_new QFont "Times" 18 (#_Bold "QFont")))
    (#_connect "QObject"
	       quit (QSIGNAL "clicked()")
	       *qapp* (QSLOT "quit()"))))

(defun main ()
  (setf *qapp* (make-qapplication))
  (let ((widget (make-instance 'my-widget)))
    (#_show widget)
    (unwind-protect
	 (#_exec *qapp*)
      (#_hide widget))))
