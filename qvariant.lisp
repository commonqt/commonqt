(in-package :qt)
#+sbcl (declaim (optimize (debug 2)))
(named-readtables:in-readtable :qt)


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
