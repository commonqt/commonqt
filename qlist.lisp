(in-package :qt)
(named-readtables:in-readtable :qt)

(defun qlist-element-type (type)
  (let* ((element-type (subseq type (1+ (position #\< type))
                               (position #\> type)))
         (star-p (find #\* element-type :from-end t)))
    (values element-type
            star-p)))

(defvar *immediate-types* '("int" "QByteArray" "QVariant"))

(defun unmarshal-qlist (type)
  (multiple-value-bind (element-type pointer) (qlist-element-type type)
    (let* ((element-qtype (find-qtype element-type))
           (element-unmarshaller (unmarshaller-2 element-qtype)))
      (compile nil
               `(lambda (qlist type)
                  (declare (ignore type))
                  (loop for i below (sw_qlist_void_size qlist)
                        collect (funcall ,element-unmarshaller
                                         ,(if (or pointer
                                                  (not (member element-type
                                                               *immediate-types*
                                                               :test #'string=)))
                                              '(sw_qlist_void_at qlist i)
                                              '(sw_qlist_scalar_at qlist i))
                                         ,element-qtype)))))))
