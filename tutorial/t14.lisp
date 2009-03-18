;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; http://doc.trolltech.com/4.3/tutorial-t14.html

(defpackage :qt-tutorial-14
  (:use :cl :qt)
  (:export #:main))

(in-package :qt-tutorial-14)
(named-readtables:in-readtable :qt)

(defvar *qapp*)

(defclass cannon-field ()
    ((current-angle :initform 45
                    :accessor current-angle)
     (current-force :initform 0
                    :accessor current-force)
     (timer-count :initform 0
                  :accessor timer-count)
     (auto-shoot-timer :accessor auto-shoot-timer)
     (shoot-angle :initform 0
                  :accessor shoot-angle)
     (shoot-force :initform 0
                  :accessor shoot-force)
     (target :accessor target)
     (game-ended-p :initform nil
                   :accessor game-ended-p)
     (barrel-pressed-p :initform nil
                       :accessor barrel-pressed-p))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("setAngle(int)" (lambda (this newval)
                             (setf (current-angle this)
                                   (min (max 5 newval) 70))))
          ("setForce(int)" (lambda (this newval)
                             (setf (current-force this)
                                   (max 0 newval))))
          ("void moveShot()" move-shot)
          ("void shoot()" shoot)
          ("void setGameOver()" set-game-over)
          ("void restartGame()" restart-game))
  (:signals ("angleChanged(int)")
            ("forceChanged(int)")
            ("void hit()")
            ("void missed()")
            ("void canShoot(bool)"))
  (:override ("paintEvent" paint-event)
             ("mousePressEvent" mouse-press-event)
             ("mouseMoveEvent" mouse-move-event)
             ("mouseReleaseEvent" mouse-release-event)))

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

(defun barrier-rect (instance)
  (#_new QRect 145 (- (#_height instance) 100) 15 99))

(defun barrel-hit-p (instance pos)
  (let ((matrix (#_new QMatrix)))
    (#_translate matrix 0 (#_height instance))
    (#_rotate matrix (- (current-angle instance)))
    (#_contains (barrel-rect) (#_map (#_inverted matrix) pos))))

(defun target-rect (instance)
  (let ((result (#_new QRect 0 0 20 10)))
    (#_moveCenter result (#_new QPoint
                                (#_x (target instance))
                                (- (#_height instance)
                                   1
                                   (#_y (target instance)))))
    result))

(defun barrel-rect ()
  (#_new QRect 30 -5 20 10))

(defun shot-rect (instance)
  (let* ((gravity 4.0d0)
         (time (/ (timer-count instance) 20.0d0))
         (velocity (shoot-force instance))
         (radians (* (shoot-angle instance) (/ pi 180.0d0)))
         (velx (* velocity (cos radians)))
         (vely (* velocity (sin radians)))
         (barrelRect (barrel-rect))
         (x0 (* (+ (#_right barrelRect) 5.0d0) (cos radians)))
         (y0 (* (+ (#_right barrelRect) 5.0d0) (sin radians)))
         (x (+ x0 (* velx time)))
         (y (+ y0 (* vely time) (- (* 0.5d0 gravity time time))))
         (result (#_new QRect 0 0 6 6)))
    (#_moveCenter result (#_new QPoint
                              (round x)
                              (- (#_height instance) 1 (round y))))
    result))

(defun is-shooting-p (instance)
  (#_isActive (auto-shoot-timer instance)))

(defun shoot (instance)
  (unless (is-shooting-p instance)
    (setf (timer-count instance) 0)
    (setf (shoot-angle instance) (current-angle instance))
    (setf (shoot-force instance) (current-force instance))
    (#_start (auto-shoot-timer instance) 5)
    (emit-signal instance "canShoot(bool)" nil)))

(defun set-game-over (instance)
  (unless (game-ended-p instance)
    (when (is-shooting-p instance)
      (#_stop (auto-shoot-timer instance)))
    (setf (game-ended-p instance) t)
    (#_update instance)))

(defun restart-game (instance)
  (when (is-shooting-p instance)
    (#_stop (auto-shoot-timer instance)))
  (setf (game-ended-p instance) nil)
  (#_update instance)
  (emit-signal instance "canShoot(bool)" t))

(defun move-shot (instance)
  (let ((old (shot-rect instance)))
    (incf (timer-count instance))
    (let ((new (shot-rect instance)))
      (cond
        ((#_intersects new (target-rect instance))
         (#_stop (auto-shoot-timer instance))
         (emit-signal instance "hit()")
         (emit-signal instance "canShoot(bool)" t))
        ((or (> (#_x new) (#_width instance))
             (> (#_y new) (#_height instance))
             (#_intersects new (barrier-rect instance)))
         (#_stop (auto-shoot-timer instance))
         (emit-signal instance "missed()")
         (emit-signal instance "canShoot(bool)" t))
        (t
         (setf old (#_unite old new)))))
    (#_update instance old)))

(defmethod initialize-instance :after ((instance cannon-field) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (setf (auto-shoot-timer instance) (#_new QTimer instance))
  (#_connect "QObject"
             (auto-shoot-timer instance)
             (QSIGNAL "timeout()")
             instance
             (QSLOT "moveShot()"))
  (#_setPalette instance (#_new QPalette (#_new QColor 250 250 200)))
  (#_setAutoFillBackground instance (bool 1))
  (new-target instance))

(defun new-target (instance)
  (setf (target instance)
        (#_new QPoint
               (+ 200 (random 190))
               (+ 10 (random 255))))
  (#_update instance))

(defun paint-shot (instance painter)
  (#_setPen painter (#_NoPen "Qt"))
  (#_setBrush painter (#_new QBrush (#_black "Qt") (#_SolidPattern "Qt")))
  (#_drawRect painter (shot-rect instance)))

(defun paint-cannon (instance painter)
  (#_setPen painter (#_NoPen "Qt"))
  (#_setBrush painter (#_new QBrush (#_blue "Qt") (#_SolidPattern "Qt")))

  (#_save painter)
  (#_translate painter 0 (#_height (#_rect instance)))
  (#_drawPie painter (#_new QRect -35 -35 70 70) 0 (* 90 16))
  (#_rotate painter (- (current-angle instance)))
  (#_drawRect painter (#_new QRect 30 -5 20 10))
  (#_restore painter))

(defun paint-target (instance painter)
  (#_setPen painter (#_NoPen "Qt"))
  (#_setBrush painter (#_new QBrush (#_red "Qt") (#_SolidPattern "Qt")))
  (#_drawRect painter (target-rect instance)))

(defun paint-barrier (instance painter)
  (#_setPen painter (#_new QPen (#_black "Qt")))
  (#_setBrush painter (#_new QBrush (#_yellow "Qt") (#_SolidPattern "Qt")))
  (#_drawRect painter (barrier-rect instance)))

(defmethod paint-event ((instance cannon-field) paint-event)
  (let ((painter (#_new QPainter instance)))
    (when (game-ended-p instance)
      (#_setPen painter (#_black "Qt"))
      (#_setFont painter (#_new QFont "Courier" 48 (#_Bold "QFont")))
      (#_drawText painter (#_rect instance) (#_AlignCenter "Qt") "Game Over"))
    (paint-cannon instance painter)
    (when (is-shooting-p instance)
      (paint-shot instance painter))
    (unless (game-ended-p instance)
      (paint-target instance painter))
    (paint-barrier instance painter)
    (#_end painter)))

(defmethod mouse-press-event ((instance cannon-field) event)
  (setf (barrel-pressed-p instance)
        (and (enum= (#_button event) (#_LeftButton "Qt"))
             (barrel-hit-p instance (#_pos event)))))

(defmethod mouse-move-event ((instance cannon-field) event)
  (when (barrel-pressed-p instance)
    (let ((pos (#_pos event)))
      (unless (plusp (#_x pos))
        (#_setX pos 1))
      (unless (< (#_x pos) (#_height instance))
        (#_setX pos (1- (#_height instance))))
      (let ((radians (atan (- (float (#_bottom (#_rect instance)) 1.0d0)
                              (#_y pos))
                           (#_x pos))))
        (setf (current-angle instance)
              (round (* radians (/ 180 pi))))))))

(defmethod mouse-release-event ((instance cannon-field) event)
  (when (enum= (#_button event) (#_LeftButton "Qt"))
    (setf (barrel-pressed-p instance) nil)))

(defclass lcd-range ()
    ((slider :accessor slider)
     (label :accessor label))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("setValue(int)" (lambda (this int) (setf (value this) int)))
          ("setRange(int,int)" set-range))
  (:signals ("valueChanged(int)")))

(defmethod value ((instance lcd-range))
  (#_value (slider instance)))

(defmethod (setf value) (newval (instance lcd-range))
  (#_setValue (slider instance) newval))

(defmethod text ((instance lcd-range))
  (#_text (label instance)))

(defmethod (setf text) (newval (instance lcd-range))
  (#_setText (label instance) newval))

(defun set-range (instance min max)
  (when (or (minusp min) (> max 99) (> min max))
    (warn "invalid SET-RANGE(~D, ~D)" min max))
  (#_setRange (slider instance) min max))

(defmethod initialize-instance
    :after
    ((instance lcd-range) &key parent text)
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
      (let ((label (#_new QLabel)))
        (#_setSizePolicy label
                         (#_Preferred "QSizePolicy")
                         (#_Fixed "QSizePolicy"))
        (setf (label instance) label)
        (#_setAlignment label (logior (primitive-value
                                       (#_AlignHCenter "Qt"))
                                      (primitive-value
                                       (#_AlignTop "Qt"))))
        (let ((layout (#_new QVBoxLayout)))
          (#_addWidget layout lcd)
          (#_addWidget layout slider)
          (#_addWidget layout label)
          (#_setLayout instance layout)))
      (#_setFocusProxy instance slider)))
  (when text
    (setf (text instance) text)))

(defclass game-board ()
    ((hits :accessor hits)
     (shots-left :accessor shots-left)
     (cannon-field :accessor cannon-field))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("fire()" fire)
          ("hit()" hit)
          ("missed()" missed)
          ("newGame()" new-game)))

(defmethod initialize-instance :after ((instance game-board) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (let ((quit (#_new QPushButton "&Quit"))
        (shoot (#_new QPushButton "&Shoot"))
        (new-game (#_new QPushButton "&New Game"))
        (font (#_new QFont (qstring "Times") 18 (#_Bold "QFont"))))
    (#_setFont quit font)
    (#_setFont shoot font)
    (#_setFont new-game font)
    (#_connect "QObject"
                new-game
                (QSIGNAL "clicked()")
                instance
                (QSLOT "newGame()"))
    (#_connect "QObject"
                quit
                (QSIGNAL "clicked()")
                *qapp*
                (QSLOT "quit()"))
    (let ((angle (make-instance 'lcd-range :text "ANGLE"))
          (force (make-instance 'lcd-range :text "FORCE"))
          (hits (#_new QLCDNumber 2))
          (shots-left (#_new QLCDNumber 2))
          (hits-label (#_new QLabel "HITS"))
          (shots-left-label (#_new QLabel "SHOTS LEFT"))
          (cf (make-instance 'cannon-field))
          (cannon-box (#_new QFrame)))
      (#_setFrameStyle cannon-box
                       (logior (primitive-value (#_WinPanel "QFrame"))
                               (primitive-value (#_Sunken "QFrame"))))
      (#_new QShortcut
             (#_new QKeySequence (#_Key_Enter "Qt"))
             instance
             (QSLOT "fire()"))
      (#_new QShortcut
             (#_new QKeySequence (#_Key_Return "Qt"))
             instance
             (QSLOT "fire()"))
      (#_new QShortcut
             (#_new QKeySequence (#_CTRL "Qt") (#_Key_Q "Qt"))
             instance
             (QSLOT "close()"))
      (setf (cannon-field instance) cf)
      (setf (hits instance) hits)
      (setf (shots-left instance) shots-left)
      (#_setSegmentStyle hits (#_Filled "QLCDNumber"))
      (#_setSegmentStyle shots-left (#_Filled "QLCDNumber"))
      (#_connect "QObject"
                 shoot
                 (QSIGNAL "clicked()")
                 instance
                 (QSLOT "fire()"))
      (#_connect "QObject"
                 cf
                 (QSIGNAL "hit()")
                 instance
                 (QSLOT "hit()"))
      (#_connect "QObject"
                 cf
                 (QSIGNAL "missed()")
                 instance
                 (QSLOT "missed()"))
      (#_connect "QObject"
                 cf
                 (QSIGNAL "canShoot(bool)")
                 shoot
                 (QSLOT "setEnabled(bool)"))
      (set-range angle 5 70)
      (set-range force 10 50)
      (#_connect "QObject"
                  angle
                  (QSIGNAL "valueChanged(int)")
                  cf
                  (QSLOT "setAngle(int)"))
      (#_connect "QObject"
                  cf
                  (QSIGNAL "angleChanged(int)")
                  angle
                  (QSLOT "setValue(int)"))
      (#_connect "QObject"
                  force
                  (QSIGNAL "valueChanged(int)")
                  cf
                  (QSLOT "setForce(int)"))
      (#_connect "QObject"
                  cf
                  (QSIGNAL "forceChanged(int)")
                  force
                  (QSLOT "setValue(int)"))
      (let ((left-layout (#_new QVBoxLayout))
            (top-layout (#_new QHBoxLayout))
            (grid (#_new QGridLayout)))
        (#_addWidget left-layout angle)
        (#_addWidget left-layout force)

        (#_addWidget top-layout shoot)
        (#_addWidget top-layout hits)
        (#_addWidget top-layout hits-label)
        (#_addWidget top-layout shots-left)
        (#_addWidget top-layout shots-left-label)
        (#_addStretch top-layout 1)
        (#_addWidget top-layout new-game)

        (#_addWidget grid quit 0 0)
        (#_addLayout grid top-layout 0 1)
        (#_addLayout grid left-layout 1 0)
        (#_addWidget grid cannon-box 1 1 2 1)
        (#_addWidget grid cf 1 1 2 1)
        (#_setColumnStretch grid 1 10)
        (#_setLayout instance grid))
      (setf (value angle) 60)
      (setf (value force) 25)
      (#_setFocus angle)
      (new-game instance))))

(defmethod fire ((game game-board))
  (with-slots (cannon-field shots-left) game
    (unless (or (game-ended-p cannon-field)
                (is-shooting-p cannon-field))
      (#_display shots-left (1- (#_intValue shots-left)))
      (shoot cannon-field))))

(defmethod hit ((game game-board))
  (with-slots (cannon-field hits shots-left) game
    (#_display hits (1+ (#_intValue hits)))
    (if (zerop (#_intValue shots-left))
        (set-game-over cannon-field)
        (new-target cannon-field))))

(defmethod missed ((game game-board))
  (with-slots (cannon-field shots-left) game
    (when (zerop (#_intValue shots-left))
      (set-game-over cannon-field))))

(defmethod new-game ((game game-board))
  (with-slots (cannon-field hits shots-left) game
    (#_display shots-left 15)
    (#_display hits 0)
    (restart-game cannon-field)
    (new-target cannon-field)))

(defun main ()
  (setf *qapp* (make-qapplication))
  (let ((window (make-instance 'game-board)))
    (#_setGeometry window 100 100 500 355)
    (#_show window)
    (unwind-protect
         (#_exec *qapp*)
      (#_hide window))))
