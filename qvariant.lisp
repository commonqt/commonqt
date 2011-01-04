(in-package :qt)
(named-readtables:in-readtable :qt)

(defmarshal (value :|QVariant|)
  (qvariant value))

(defmarshal (value :|const QVariant&|)
  (qvariant value))

(defun qvariant-ptr-types ()
  (with-cache ()
    (iter (for (code . type) in (list (cons (#_QVariant::Color) "QColor")
                                      (cons (#_QVariant::Pixmap) "QPixmap")
                                      (cons (#_QVariant::Icon) "QIcon")))
          (collect (cons (primitive-value code)
                         (find-qclass type))))))

(defun qvariant (value)
  (etypecase value
    (string (#_new QVariant :|const QString&| value))
    (integer (#_new QVariant :|int| value))
    ((or single-float double-float) (#_new QVariant :|double| value))
    (qobject
       (iter (for (code . type) in (qvariant-ptr-types))
             (when (qtypep value type)
               (return (#_new QVariant code (qobject-pointer value))))
             (finally (return value))))))

(defun unvariant (variant &optional (type (find-qtype "QVariant")))
  (let* ((qobject (%qobject (qtype-class type) variant))
         (code (primitive-value (#_type qobject))))
    (case code
      (2 (#_toInt qobject))
      (10 (#_toString qobject))
      (6 (#_toDouble qobject))
      (t
         (alexandria:if-let ((qclass (cdr (assoc code (qvariant-ptr-types)))))
           (%qobject qclass (#_constData qobject))
           qobject)))))

(define-marshalling-test (value :|QVariant|)
  ;; FIXME: this belongs to qvariant.lisp but we need it here (and qvariant.lisp needs call stuff)
  (typecase value
    ((or string integer single-float double-float) t)
    (qobject
     (or (qtypep value "QVariant")
         (iter (for (code . type) in (qvariant-ptr-types))
               (thereis (qtypep value type)))))
    (t nil)))
