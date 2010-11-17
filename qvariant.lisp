(in-package :qt)
(named-readtables:in-readtable :qt)

(defmarshal (value :|QVariant|)
  (qvariant value))

(defmarshal (value :|const QVariant&|)
  (qvariant value))

(defun qvariant (value)
  (etypecase value
    (string (#_new QVariant :|const QString&| value))
    (integer (#_new QVariant :|int| value))
    (qobject value)))

(defun unvariant (variant &optional (type (find-qtype "QVariant")))
  (let ((qobject (%qobject (qtype-class type) variant)))
    (case (primitive-value (#_type qobject))
      (2 (#_toInt qobject))
      (10 (#_toString qobject))
      (t qobject))))
