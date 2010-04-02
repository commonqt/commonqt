;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; http://doc.trolltech.com/4.3/tutorial-t3.html

(defpackage :qt-tutorial-5
  (:use :cl :qt)
  (:export #:main))

(in-package :qt-tutorial-5)
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
  (let ((quit (#_new QPushButton "Quit" instance))
	(lcd (#_new QLCDNumber 2))
	(slider (#_new QSlider (#_Horizontal "Qt"))))
    (#_setFont quit (#_new QFont "Times" 18 (#_Bold "QFont")))
    (#_setSegmentStyle lcd (#_Filled "QLCDNumber"))
    (#_setRange slider 0 99)
    (#_setValue slider 0)

    (#_connect "QObject"
	       quit (QSIGNAL "clicked()")
	       *qapp* (QSLOT "quit()"))
    (#_connect "QObject"
	       slider (QSIGNAL "valueChanged(int)")
	       lcd (QSLOT "display(int)"))

    (let ((layout (#_new QVBoxLayout)))
      (#_addWidget layout quit)
      (#_addWidget layout lcd)
      (#_addWidget layout slider)
      (#_setLayout instance layout))))

(defun main ()
  (setf *qapp* (make-qapplication))
  (let ((widget (make-instance 'my-widget)))
    (#_show widget)
    (unwind-protect
	 (#_exec *qapp*)
      (#_hide widget))))
