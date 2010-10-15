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

(defun unvariant (variant type)
  (let ((qobject (%qobject (qtype-class type) variant)))
    (case (primitive-value (#_type qobject))
      (2 (#_toInt qobject))
      (t qobject))))
