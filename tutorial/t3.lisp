;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; http://doc.trolltech.com/4.3/tutorial-t3.html

(defpackage :qt-tutorial-3
  (:use :cl :qt)
  (:export #:main))

(in-package :qt-tutorial-3)
(named-readtables:in-readtable :qt)

(defvar *qapp*)

(defun main ()
  (setf *qapp* (make-qapplication))

  (let ((window (#_new QWidget)))
    (#_resize window 200 120)

    (let ((quit (#_new QPushButton "Quit" window)))
      (#_setFont quit (#_new QFont "Times" 18 (#_Bold "QFont")))
      (#_setGeometry quit 10 40 180 40)
      (#_connect "QObject"
		 quit (QSIGNAL "clicked()")
		 *qapp* (QSLOT "quit()"))

      (#_show window)
      (unwind-protect
	   (#_exec *qapp*)
	(#_hide window)))))
