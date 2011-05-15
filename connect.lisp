;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; Copyright (c) 2009 David Lichteblau. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :qt)
(named-readtables:in-readtable :qt)

;; dynamic slot mechanism is implemented according to
;; http://doc.trolltech.com/qq/qq16-dynamicqobject.html

(defun QSIGNAL (str)
  (concatenate 'string "2" str))

(defun QSLOT (str)
  (concatenate 'string "1" str))

(defclass dynamic-receiver ()
  ((slots :accessor dynamic-receiver-slots :initform (make-hash-table))
   (connections :accessor dynamic-receiver-connections :initform '())
   (next-id :accessor dynamic-receiver-next-id))
  (:qt-superclass "QObject")
  (:metaclass qt-class))

(defstruct (connection-entry
             (:type list)
             (:constructor make-connection-entry (sender signal-id function slot-id)))
  sender signal-id function slot-id)

(defmethod initialize-instance :after ((instance dynamic-receiver) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (setf (dynamic-receiver-next-id instance)
        (length (class-member-table (class-of instance)))))

(defmethod dynamic-object-member ((object dynamic-receiver) id)
  (values (gethash id (dynamic-receiver-slots object))))

(defun resolve-signal (sender signal)
  (let ((signal-sig (#_data (#_QMetaObject::normalizedSignature
                             (if (alexandria:starts-with #\2 signal)
                                 (subseq signal 1)
                                 signal)))))
    (values (#_indexOfSignal (#_metaObject sender) signal-sig)
            signal-sig)))

(defun sweep-connections (receiver)
  (setf (dynamic-receiver-connections receiver)
        (delete-if-not #'(lambda (item)
                           (tg:weak-pointer-value (connection-entry-sender item)))
                       (dynamic-receiver-connections receiver))))

(defun dynamic-connect (receiver sender signal function this-object)
  (sweep-connections receiver)
  (multiple-value-bind (signal-id signal-sig)
      (resolve-signal sender signal)
    (let* ((slot-id (dynamic-receiver-next-id receiver))
           (slot-sig (cl-ppcre:regex-replace
                      "^(\\d+)[^(]+" signal-sig
                      (format nil "\\1dynamicSlot~A" slot-id))))
      (assert (#_QMetaObject::checkConnectArgs signal-sig slot-sig))
      (setf (gethash slot-id (dynamic-receiver-slots receiver))
            (make-instance 'slot-member
                           :name (subseq slot-sig 1)
                           :function
                           (if this-object
                               #'(lambda (this &rest args)
                                   (declare (ignore this))
                                   (apply function this-object args))
                               #'(lambda (this &rest args)
                                   (declare (ignore this))
                                   (apply function args)))))
      (push (make-connection-entry (tg:make-weak-pointer sender) signal-id function slot-id)
            (dynamic-receiver-connections receiver))
      (incf (dynamic-receiver-next-id receiver))
      (#_QMetaObject::connect sender signal-id receiver
                              (+ slot-id (#_methodCount (#_metaObject receiver)))))))

(defun dynamic-disconnect (receiver sender signal function)
  (sweep-connections receiver)
  (let* ((signal-id (resolve-signal sender signal))
         (connection
          (iter (for connection in (dynamic-receiver-connections receiver))
                (when (and (eq sender (tg:weak-pointer-value
                                       (connection-entry-sender connection)))
                           (= signal-id (connection-entry-signal-id connection))
                           (eq function (connection-entry-function connection)))
                  (return connection))
                (finally
                 (error "unable to locate dynamic connection, sender ~s, ~
                              receiver ~s, signal ~s, function ~s"
                        sender receiver signal function)))))
    (#_QMetaObject::disconnect
     sender signal-id receiver
     (+ (connection-entry-slot-id connection)
        (#_methodCount (#_metaObject receiver))))
    (alexandria:deletef (dynamic-receiver-connections receiver) connection)))

(defun ensure-dynamic-receiver (owner)
  (or (iter (for child in (#_children owner))
            (finding child such-that (typep child 'dynamic-receiver)))
      (make-instance 'dynamic-receiver :parent owner)))

(defun parse-connect-args (sender signal destination)
  (unless (plusp (length signal))
    (error "invalid signal name"))
  (setf signal (if (digit-char-p (char signal 0))
                   signal
                   (QSIGNAL signal)))
  (unless (<= 1 (length destination) 2)
    (error "invalid connection destination"))
  (cond ((alexandria:length= 1 destination)
         (values t signal
                 (ensure-dynamic-receiver sender)
                 (first destination) nil))
        ((stringp (second destination))
         (values nil
                 signal
                 (first destination)
                 (let ((slot (second destination)))
                   (if (digit-char-p (char slot 0))
                       slot
                       (QSLOT slot))) nil))
        (t
         (unless (functionp (second destination))
           (error "dynamic connection spec refers to a non-function object"))
         (values t signal
                 (ensure-dynamic-receiver (first destination))
                 (second destination)
                 (first destination)))))

(defun connect (sender signal &rest destination)
  "Connect the SIGNAL of the SENDER to the specified destination.
  There are three ways to call CONNECT:
  1. (connect SENDER SIGNAL RECEIVER SLOT)
  Connect named SIGNAL of SENDER to a named SLOT of the RECEIVER
  2. (connect SENDER SIGNAL FUNCTION)
  Connect named SIGANL of SENDER to FUNCTION. The FUNCTION
  receives signal arguments as its arguments.
  3. (connect SENDER SIGNAL RECEIVER FUNCTION)
  Connect named SIGANL of SENDER to FUNCTION. The FUNCTION
  receives RECEIVER followed by signal arguments as its arguments.
  The connection is removed as soon as RECEIVER object is deleted.

  In all of above cases, wrapping signal and slot names in
  (QSIGNAL ...) and (QSLOT ...) is not required, although it's
  not an error. The only case in which it's necessary is
  connecting a signal to another signal, in which (QSIGNAL ...)
  is used in place of SLOT."
  (multiple-value-bind (dynamic-p signal receiver target this-object)
      (parse-connect-args sender signal destination)
    (if dynamic-p
        (dynamic-connect receiver sender signal target this-object)
        (#_connect "QObject" sender signal receiver target))))

(defun disconnect (sender signal &rest destination)
  "Disconnect the SIGNAL of the SENDER from the specified destination.
  The destination can be specified as a RECEIVER SLOT, FUNCTION
  or RECEIVER FUNCTION."
  (multiple-value-bind (dynamic-p signal receiver target)
      (parse-connect-args sender signal destination)
    (if dynamic-p
        (dynamic-disconnect receiver sender signal target)
        (#_disconnect "QObject" sender signal receiver target))))

(defmacro with-signals-blocked (object &body body)
  "Execute BODY while signals emitted by OBJECT are blocked."
  (let ((object-var (gensym "OBJECT")))
    `(let ((,object-var ,object))
       (unwind-protect (progn (optimized-call nil ,object-var "blockSignals" t)
                              ,@body)
         (optimized-call nil ,object-var "blockSignals" nil)))))
