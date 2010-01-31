;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; http://doc.trolltech.com/4.3/tutorial-t10.html

(defpackage :qt-tutorial-10
  (:use :cl :qt)
  (:export #:main))

(in-package :qt-tutorial-10)
(named-readtables:in-readtable :qt)

(defvar *qapp*)

(defclass cannon-field ()
    ((current-angle :initform 45
                    :accessor current-angle)
     (current-force :initform 0
                    :accessor current-force))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("setAngle(int)" (lambda (this newval)
                             (setf (current-angle this)
                                   (min (max 5 newval) 70))))
          ("setForce(int)" (lambda (this newval)
                             (setf (current-force this)
                                   (max 0 newval)))))
  (:signals ("angleChanged(int)")
            ("forceChanged(int)"))
  (:override ("paintEvent" paint-event)))

(defmethod (setf current-angle) :around (newval (instance cannon-field))
  (let ((oldval (current-angle instance)))
    (prog1
        (call-next-method)
      (unless (eql oldval newval)
        (#_update instance (cannon-rect instance))
        (emit-signal instance "angleChanged(int)" newval)))))

(defmethod (setf current-force) :around (newval (instance cannon-field))
  (let ((oldval (current-force instance)))
    (prog1
        (call-next-method)
      (unless (eql oldval newval)
        (#_update instance (cannon-rect instance))
        (emit-signal instance "forceChanged(int)" newval)))))

(defun cannon-rect (instance)
  (let ((result (#_new QRect 0 0 50 50)))
    (#_moveBottomLeft result (#_bottomLeft (#_rect instance)))
    result))

(defmethod initialize-instance :after ((instance cannon-field) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (#_setPalette instance (#_new QPalette (#_new QColor 250 250 200)))
  (#_setAutoFillBackground instance (bool 1)))

(defmethod paint-event ((instance cannon-field) paint-event)
  (let ((painter (#_new QPainter instance)))
    (#_setPen painter (#_NoPen "Qt"))
    (#_setBrush painter (#_new QBrush (#_blue "Qt") (#_SolidPattern "Qt")))
    (#_translate painter 0 (#_height (#_rect instance)))
    (#_drawPie painter (#_new QRect -35 -35 70 70) 0 (* 90 16))
    (#_rotate painter (- (current-angle instance)))
    (#_drawRect painter (#_new QRect 30 -5 20 10))
    (#_end painter)))

(defclass lcd-range ()
    ((slider :accessor slider))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("setValue(int)" (lambda (this int) (setf (value this) int)))
          ("setRange(int,int)" set-range))
  (:signals ("valueChanged(int)")))

(defmethod value ((instance lcd-range))
  (#_value (slider instance)))

(defmethod (setf value) (newval (instance lcd-range))
  (#_setValue (slider instance) newval))

(defun set-range (instance min max)
  (when (or (minusp min) (> max 99) (> min max))
    (warn "invalid SET-RANGE(~D, ~D)" min max))
  (#_setRange (slider instance) min max))

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
        (#_setLayout instance layout))
      (#_setFocusProxy instance slider))))

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
    (let ((angle (make-instance 'lcd-range))
          (force (make-instance 'lcd-range))
          (cannon-field (make-instance 'cannon-field)))
      (set-range angle 5 70)
      (set-range force 10 50)
      (#_connect "QObject"
                  angle
                  (QSIGNAL "valueChanged(int)")
                  cannon-field
                  (QSLOT "setAngle(int)"))
      (#_connect "QObject"
                  cannon-field
                  (QSIGNAL "angleChanged(int)")
                  angle
                  (QSLOT "setValue(int)"))
      (#_connect "QObject"
                  force
                  (QSIGNAL "valueChanged(int)")
                  cannon-field
                  (QSLOT "setForce(int)"))
      (#_connect "QObject"
                  cannon-field
                  (QSIGNAL "forceChanged(int)")
                  force
                  (QSLOT "setValue(int)"))
      (let ((left-layout (#_new QVBoxLayout)))
        (#_addWidget left-layout angle)
        (#_addWidget left-layout force)
        (let ((grid (#_new QGridLayout)))
          (#_addWidget grid quit 0 0)
          (#_addLayout grid left-layout 1 0)
          (#_addWidget grid cannon-field 1 1 2 1)
          (#_setColumnStretch grid 1 10)
          (#_setLayout instance grid)))
      (setf (value angle) 60)
      (setf (value force) 25)
      (#_setFocus angle))))

(defun main ()
  (setf *qapp* (make-qapplication))
  (let ((window (make-instance 'my-widget)))
    (#_setGeometry window 100 100 500 355)
    (#_show window)
    (unwind-protect
         (#_exec *qapp*)
      (#_hide window))))
