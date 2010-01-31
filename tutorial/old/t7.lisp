;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; http://doc.trolltech.com/4.3/tutorial-t7.html

(defpackage :qt-tutorial-7
  (:use :cl :qt)
  (:export #:main))

(in-package :qt-tutorial-7)
(named-readtables:in-readtable :qt)

(defvar *qapp*)

(defclass lcd-range ()
    ((slider :accessor slider))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("setValue(int)" (lambda (this int) (setf (value this) int))))
  (:signals ("valueChanged(int)")))

(defun value (instance)
  (#_value (slider instance)))

(defun (setf value) (newval instance)
  (#_setValue (slider instance) newval))

(defmethod initialize-instance :after ((instance lcd-range) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (let ((lcd (#_new QLCDNumber 2)))
    (#_setSegmentStyle lcd (#_Filled "QLCDNumber"))
    (let ((slider (#_new QSlider (#_Horizontal "Qt"))))
      (setf (slider instance) slider)
      (#_setRange slider 0 99)
      (#_setValue slider 0)
      (#_connect "QObject"
                  slider
                  (QSIGNAL "valueChanged(int)")
                  lcd
                  (QSLOT "display(int)"))
      (#_connect "QObject"
                  slider
                  (QSIGNAL "valueChanged(int)")
                  instance
                  (QSIGNAL "valueChanged(int)"))
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
    (let ((grid (#_new QGridLayout))
          (prev nil))
      (dotimes (row 3)
        (dotimes (column 3)
          (let ((range (make-instance 'lcd-range)))
            (#_addWidget grid range row column)
            (when prev
              (#_connect "QObject"
                          range
                          (QSIGNAL "valueChanged(int)")
                          prev
                          (QSLOT "setValue(int)")))
            (setf prev range))))
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
