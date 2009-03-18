;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; http://doc.trolltech.com/4.3/tutorial-t2.html

(defpackage :qt-tutorial-2
  (:use :cl :qt)
  (:export #:main))

(in-package :qt-tutorial-2)
(named-readtables:in-readtable :qt)

(defvar *qapp*)

(defun main ()
  (setf *qapp* (make-qapplication))
  (let ((quit (#_new QPushButton "Quit")))
    (#_resize quit 75 30)
    (#_setFont quit (#_new QFont "Times" 18 (#_Bold "QFont")))
    (#_connect "QObject"
	       quit (QSIGNAL "clicked()")
	       *qapp* (QSLOT "quit()"))
    (#_show quit)
    (unwind-protect
         (#_exec *qapp*)
      (#_hide quit))))
