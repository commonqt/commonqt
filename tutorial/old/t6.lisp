;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; http://doc.trolltech.com/4.3/tutorial-t6.html

(defpackage :qt-tutorial-6
  (:use :cl :qt)
  (:export #:main))

(in-package :qt-tutorial-6)
(named-readtables:in-readtable :qt)

(defvar *qapp*)

(defclass lcd-range ()
    ()
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((instance lcd-range) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (let ((lcd (#_new QLCDNumber 2)))
    (#_setSegmentStyle lcd (#_Filled "QLCDNumber"))
    (let ((slider (#_new QSlider (#_Horizontal "Qt"))))
      (#_setRange slider 0 99)
      (#_setValue slider 0)
      (#_connect "QObject"
                  slider
                  (QSIGNAL "valueChanged(int)")
                  lcd
                  (QSLOT "display(int)"))
      (let ((layout (#_new QVBoxLayout)))
        (#_addWidget layout lcd)
        (#_addWidget layout slider)
        (#_setLayout instance layout)))))

(defclass my-widget ()
    ()
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((instance my-widget) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (let ((quit (#_new QPushButton (qstring "Quit"))))
    (#_setFont quit (#_new QFont (qstring "Times") 18 (#_Bold "QFont")))
    (#_connect "QObject"
                quit
                (QSIGNAL "clicked()")
                *qapp*
                (QSLOT "quit()"))
    (let ((grid (#_new QGridLayout)))
      (dotimes (row 3)
        (dotimes (column 3)
          (#_addWidget grid
                        (make-instance 'lcd-range)
                        row
                        column)))
      (let ((layout (#_new QVBoxLayout)))
        (#_addWidget layout quit)
        (#_addLayout layout grid)
        (#_setLayout instance layout)))))

(defun main ()
  (setf *qapp* (make-qapplication))
  (let ((window (make-instance 'my-widget)))
    (#_show window)
    (unwind-protect
         (#_exec *qapp*)
      (#_hide window))))
