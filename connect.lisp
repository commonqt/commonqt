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
        (length (slot-or-signal-table (class-of instance)))))

(defmethod dynamic-slot-or-signal ((object dynamic-receiver) id)
  (values (gethash id (dynamic-receiver-slots object))))

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
                      (format nil "\\1dynamicSlot~A" slot-id)))
           (spec (make-instance 'slot-spec
                                :name slot-sig
                                :function
                                (if this-object
                                    (lambda (this &rest args)
                                      (declare (ignore this))
                                      (apply function this-object args))
                                    (lambda (this &rest args)
                                      (declare (ignore this))
                                      (apply function args))))))
      (assert (#_QMetaObject::checkConnectArgs signal-sig slot-sig))
      (initialize-slot-or-signal spec)
      (setf (gethash slot-id (dynamic-receiver-slots receiver))
            spec)
      (push (make-connection-entry (tg:make-weak-pointer sender)
                                   signal-id function slot-id)
            (dynamic-receiver-connections receiver))
      (incf (dynamic-receiver-next-id receiver))
      (#_QMetaObject::connect sender signal-id receiver
                              (+ slot-id (#_methodCount
                                          (qobject-metaobject receiver)))))))

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
                 (error "Unable to locate dynamic connection, sender ~s, ~
                              receiver ~s, signal ~s, function ~s"
                        sender receiver signal function)))))
    (#_QMetaObject::disconnect
     sender signal-id receiver
     (+ (connection-entry-slot-id connection)
        (#_methodCount (qobject-metaobject receiver))))
    (alexandria:deletef (dynamic-receiver-connections receiver) connection)))

(defun ensure-dynamic-receiver (owner)
  (or (iter (for child in (#_children owner))
            (finding child such-that (typep child 'dynamic-receiver)))
      (make-instance 'dynamic-receiver :parent owner)))

(defun parse-connect-args (sender signal destination)
  (unless (and (stringp signal)
               (plusp (length signal)))
    (error "Invalid signal name: ~s" signal))
  (setf signal (if (digit-char-p (char signal 0))
                   signal
                   (QSIGNAL signal)))
  (unless (<= 1 (length destination) 2)
    (error "Invalid connection destination: ~s" destination))
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
           (error "Dynamic connection spec refers to a non-function object: ~s"
                  (second destination)))
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

(defmacro with-signals-blocked (objects &body body)
  "Execute BODY while signals emitted by OBJECT are blocked."
  (let ((vars (loop repeat (length objects)
                    collect (gensym))))
    `(let ,(mapcar #'list vars objects)
       (unwind-protect
            (progn ,@(loop for var in vars
                           collect `(#_blockSignals ,var t))
                   ,@body)
         (progn
           ,@(loop for var in vars
                   collect `(#_blockSignals ,var nil)))))))
;;;

(defun resolve-signal (sender signal &key args-length)
  (with-objects ((normalized
                  (#_QMetaObject::normalizedSignature
                   (if (alexandria:starts-with #\2 signal)
                       (subseq signal 1)
                       signal))))
    (let* ((signal-sig (#_data normalized))
           (meta (qobject-metaobject sender))
           (index (#_indexOfSignal meta signal-sig)))
      (when (minusp index)
        (error "~s doesn't have a signal named ~s" sender signal))
      (values index signal-sig
              (and args-length
                   (let ((types ;; FIXME: possible memory leaking
                           (mapcar (lambda (x) (find-signal-qtype (#_data x)))
                                   (#_parameterTypes (#_method meta index)))))
                     (when (/= args-length (length types))
                       (error "Invalid number of arguments for signal ~a: ~a"
                              signal-sig args-length))
                     types))))))

(defun call-with-signal-marshalling (fun types args)
  (let ((arg-count (length args)))
    (cffi:with-foreign-object (argv :pointer (1+ arg-count))
      (cffi:with-foreign-object (stack '|union StackItem| arg-count)
        (labels ((iterate (i rest-types rest-args)
                   (cond
                     (rest-args
                      (let* ((stack-item (cffi:mem-aref stack '|union StackItem| i))
                             (arg (car rest-args))
                             (type (car rest-types))
                             (slot-type (qtype-stack-item-slot type)))
                        (marshal arg type stack-item
                                 (lambda ()
                                   (setf (cffi:mem-aref argv :pointer (1+ i))
                                         (if (or (eql slot-type 'ptr)
                                                 (eql slot-type 'class))
                                             (cffi:mem-aref stack-item :pointer)
                                             stack-item))
                                   (iterate (1+ i)
                                     (cdr rest-types)
                                     (cdr rest-args))))))
                     (t
                      (funcall fun argv)))))
          (iterate 0 types args))))))

(defun activate-signal (object index args types)
  (call-with-signal-marshalling
   (lambda (stack)
     (#_QMetaObject::activate object index stack))
   types
   args))

(defun emit-signal (object name &rest args)
  (multiple-value-bind (index signature
                        types)
      (resolve-signal object name :args-length (length args))
    (declare (ignore signature))
    (activate-signal object index args types)))

(define-compiler-macro emit-signal (object name &rest args)
  `(let ((instance ,object)
         (name ,name)
         (args (list ,@args)))
     ,@(and (plusp (length args))
            `((declare (dynamic-extent args))))
     (multiple-value-bind (instance-qclass instance-extra-sig)
         (typecase instance
           (dynamic-object
            (values (qobject-class instance)
                    (class-generation (class-of instance))))
           (t
            (values (qobject-class instance) :instance)))
       (cached-values-bind (index signature types)
           (resolve-signal instance name :args-length ,(length args))
           ((instance-qclass :hash t)
            (instance-extra-sig)
            (name))
         (declare (ignore signature))
         (activate-signal instance index args types)))))
