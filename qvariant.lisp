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
  '("Bool" "Int" "UInt" "LongLong" "ULongLong" "Double" "Char" "Map" "List"
    "String" "StringList" "ByteArray" "BitArray" "Date" "Time" "DateTime" "Url"
    "Locale" "Rect" "RectF" "Size" "SizeF" "Line" "LineF" "Point" "PointF"
    "RegExp" "Hash" "EasingCurve"))

(defvar *qvariant-map* nil)

(defun initialize-qvariant-map ()
  (let ((new-map (make-array (1+ (length *qvariant-types*)))))
    (setf (aref new-map 0) #'identity) ;; Leave invalid QVariant as it is
    (loop for type in *qvariant-types*
          for enum = (primitive-value
                      (interpret-call "QVariant" type))
          do (setf (aref new-map enum)
                   (format nil "to~a" type)))
    new-map))

(defun %unvariant (variant type)
  (let ((function (aref *qvariant-map* type)))
    (when (stringp function)
      (setf function
            (compile nil `(lambda (x)
                            (optimized-call nil x ,function)))
            (aref *qvariant-map* type) function))
    (funcall function variant)))

(defun unvariant (variant &optional (type (find-qtype "QVariant")))
  (let* ((qobject (%qobject (qtype-class type) variant))
         (code (primitive-value (#_type qobject))))
    (unless *qvariant-map*
      (setf *qvariant-map* (initialize-qvariant-map)))
    (if (array-in-bounds-p *qvariant-map* code)
        (%unvariant qobject code)
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
