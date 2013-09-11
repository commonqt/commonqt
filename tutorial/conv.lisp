#|
 Currency conversion example written by Robert Synnott.

 http://myblog.rsynnott.com/2009/03/macos-fun-with-commonqt-qt-for-common.html
|#

(defpackage :qt-conv
  (:use :cl :qt)
  (:export :main))

(in-package :qt-conv)
(named-readtables:in-readtable :qt)

(defvar *qapp*)

(defclass my-window()
  ((dollarAmount :accessor dollarAmount)
   (conversionRate :accessor conversionRate)
   (result :accessor result))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("convert()" convert)))

(defmethod convert ((instance my-window))
  (let ((result (ignore-errors
                 (* (read-from-string (#_text (dollarAmount instance)))
                    (read-from-string (#_text (conversionRate instance)))))))
    (#_setText (result instance) (if result
                                     (princ-to-string result)
                                     "ERROR"))))

(defmethod initialize-instance :after ((instance my-window) &key)
  (new instance)
  (#_setFixedSize instance 360 170)
  (let ((convert (#_new QPushButton "Convert" instance)))
    (setf (conversionRate instance) (#_new QLineEdit "1.00" instance)
	  (dollarAmount instance) (#_new QLineEdit "0.00" instance)
	  (result instance) (#_new QLineEdit "0.00" instance))
    (#_move (#_new QLabel "Exchange Rate per $1:" instance) 20 20)
    (#_move (#_new QLabel "Dollars to Covert:" instance) 20 60)
    (#_move (#_new QLabel "Amount in other Currency:" instance) 20 100)
    (#_move (dollarAmount instance) 200 60)
    (#_move (conversionRate instance) 200 20)
    (#_move (result instance) 200 100)
    (#_move convert 220 130)
    (connect convert "clicked()" instance "convert()")))

(defun main (&optional style)
  (when style
    (#_QApplication::setStyle
     (#_QStyleFactory::create (ecase style
                                (:cde "CDE")
                                (:macintosh "Macintosh")
                                (:windows "Windows")
                                (:motif "Motif")))))
  (setf *qapp* (make-qapplication))
  (let ((window (make-instance 'my-window)))
    (#_show window)
    (unwind-protect
	 (#_exec *qapp*)
      (#_hide window))))
