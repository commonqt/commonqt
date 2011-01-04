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
    (boolean (#_new QVariant :|bool| value))
    (qobject
     (iter (for (code . type) in (qvariant-ptr-types))
           (when (qtypep value type)
             (return (#_new QVariant code (qobject-pointer value))))
           (finally (return value))))))

(defvar *qvariant-types*
  '("BitArray" "Bool" "ByteArray" "Char" "Date" "DateTime" "Double" "EasingCurve"
    "Hash" "Int" "Line" "LineF" "List" "Locale" "LongLong" "Map" "Point" "PointF"
    "Rect" "RectF" "RegExp" "Size" "SizeF" "String" "StringList" "Time" "UInt"
    "ULongLong" "Url"))

(defvar *qvariant-map* nil)

(defun initialize-qvariant-map ()
  (let ((new-map (make-array 127)))
    (loop for type in *qvariant-types*
          for enum = (primitive-value
                      (interpret-call "QVariant" type))
          do (setf (aref new-map enum)
                   (format nil "to~a" type)))
    new-map))

(defun %unvariant (variant type)
  (unless *qvariant-map*
    (setf *qvariant-map* (initialize-qvariant-map)))
  (when (array-in-bounds-p *qvariant-map* type)
    (let ((function (aref *qvariant-map* type)))
      (when (stringp function)
        (setf function
              (compile nil `(lambda (x)
                              (optimized-call nil x ,function)))
              (aref *qvariant-map* type) function))
      (when (functionp function)
        (funcall function variant)))))

(defun unvariant (variant &optional (type (find-qtype "QVariant")))
  (let* ((qobject (%qobject (qtype-class type) variant))
         (code (primitive-value (#_type qobject))))
    (or (%unvariant qobject code)
        (alexandria:if-let ((qclass (cdr (assoc code (qvariant-ptr-types)))))
          (%qobject qclass (#_constData qobject))
          qobject))))

(define-marshalling-test (value :|QVariant|)
  ;; FIXME: this belongs to qvariant.lisp but we need it here (and qvariant.lisp needs call stuff)
  (typecase value
    ((or string integer single-float double-float) t)
    (qobject
     (or (qtypep value "QVariant")
         (iter (for (code . type) in (qvariant-ptr-types))
               (thereis (qtypep value type)))))
    (t nil)))
