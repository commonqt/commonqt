(in-package :qt)
#+sbcl (declaim (optimize (debug 2)))
(named-readtables:in-readtable :qt)

(defclass qlist ()
  ((pointer :initarg :pointer
            :accessor qlist-pointer)))

(defmethod print-object ((instance qlist) stream)
  (print-unreadable-object (instance stream :type t :identity nil)
    (format stream "sized ~D at 0x~8,'0X"
	    (qlist-size instance)
	    (cffi:pointer-address (qlist-pointer instance)))))

(defun qlist-to-list (qlist)
  (iter (for i from 0 below (qlist-size qlist))
	(collect (qlist-at qlist i))))

(defun qlist-append (qlist list)
  (iter (for elt in list)
	(collect (qlist-append-elt qlist elt)))
  qlist)


;; QVariant

(defclass qlist<qvariant> (qlist)
  ())

(defun make-qlist<qvariant> ()
  (make-instance 'qlist<qvariant> :pointer (sw_qlist_variant_new)))

(defmethod qlist-size ((qlist qlist<qvariant>))
  (sw_qlist_variant_size (qlist-pointer qlist)))

(defmethod qlist-at ((qlist qlist<qvariant>) idx)
  (sw_qlist_variant_at (qlist-pointer qlist) idx))

(defmethod qlist-append-elt ((qlist qlist<qvariant>) variant)
  (assert (eql (qobject-class variant) (find-qclass "QVariant")))
  (sw_qlist_variant_append (qlist-pointer qlist) (qobject-pointer variant)))

(defmethod delete-qlist ((qlist qlist<qvariant>))
  (sw_qlist_variant_delete (qlist-pointer qlist)))


;; QVariant

(defclass qlist<int> (qlist)
  ())

(defun make-qlist<int> ()
  (make-instance 'qlist<int> :pointer (sw_qlist_int_new)))

(defmethod qlist-size ((qlist qlist<int>))
  (sw_qlist_int_size (qlist-pointer qlist)))

(defmethod qlist-at ((qlist qlist<int>) idx)
  (sw_qlist_int_at (qlist-pointer qlist) idx))

(defmethod qlist-append-elt ((qlist qlist<int>) int)
  (sw_qlist_int_append (qlist-pointer qlist) int))

(defmethod delete-qlist ((qlist qlist<int>))
  (sw_qlist_int_delete (qlist-pointer qlist)))

