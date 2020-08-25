(in-package :qt)
(named-readtables:in-readtable :qt)

(define-marshalling-test (value :|QVariant|)
  (typecase value
    ((or string integer single-float double-float
         boolean) t)
    (qobject
     (or (qtypep value "QVariant")
         (iter (for (nil . type) in (variant-map))
           (thereis (qtypep value type)))))
    (t nil)))

(defmarshal (value :|QVariant| :around cont)
  (funcall cont
           (if (qtypep value "QVariant")
               value
               (qvariant value))))

(defmarshal (value :|const QVariant&| :around cont)
  (if (qtypep value "QVariant")
      (funcall cont value)
      (let ((variant (qvariant value)))
        (unwind-protect
             (funcall cont variant)
          (#_delete variant)))))

(defmarshal-override (value (:|QVariant| :|const QVariant&|))
  (if (qtypep value "QVariant")
      value
      (qvariant value)))

;;; QVariant has conversion methods for classes in QtCore
;;; *UNVARIANT-TYPES* lists such classes and types, they can be
;;; unvarianted by calling toClass on them.  UNVARIANT-MAP builds an
;;; array from *UNVARIANT-TYPES*, indexed by QVariant type-codes and
;;; containing functions which will call appropriate toClass methods.
;;; 
;;; Classes which are from QtGui don't have such methods, so they are
;;; unvarianted by calling constData, constData returns a raw pointer,
;;; so we need to know which class it belongs to.
;;; UNVARIANT-NON-CORE-MAP makes an array from
;;; *UNVARIANT-NON-CORE-TYPES*, it's indexed by the type-code of the
;;; variant and has classes as elements.
;;; 
;;; Making a QVariant from an object is easier, just need to call
;;; (#_new QVariant type-code object). VARIANT-MAP builds a map from
;;; classes to type-codes.

(defparameter *unvariant-non-core-types*
  '("Bitmap" "Brush" "Color" "Cursor" "Font" "Icon" "Image" "KeySequence"
    "Matrix4x4" "Palette" "Pen" "Pixmap" "Polygon" "Quaternion" "Region"
    "SizePolicy" "TextFormat" "TextLength" "Transform" "Vector2D" "Vector3D"
    "Vector4D"))

(defparameter *unvariant-types*
  '("Bool" "Int" "UInt" "LongLong" "ULongLong" "Double" "Char" "Map" "List"
    "String" "StringList" "ByteArray" "BitArray" "Date" "Time" "DateTime" "Url"
    "Locale" "Rect" "RectF" "Size" "SizeF" "Line" "LineF" "Point" "PointF"
    "RegExp" "Hash" "EasingCurve"))

(defparameter *variant-types*
  (append '("ByteArray" "BitArray" "Date" "Time" "DateTime" "Url" "Char"
            "Locale" "Rect" "RectF" "Size" "SizeF" "Line" "LineF" "Point" "PointF"
            "RegExp" "EasingCurve")
          *unvariant-non-core-types*))

(defun variant-map ()
  (with-cache ()
    (loop for type in *variant-types*
          for full-name = (format nil "Q~a" type)
          for class = (find-qclass full-name nil)
          when class
          collect (cons (enum-value
                         (interpret-call "QVariant" type))
                        class))))

(defun unvariant-map ()
  (let ((new-map (make-array (1+ (length *unvariant-types*)))))
    (setf (aref new-map 0) #'identity) ;; Leave invalid QVariant as it is
    (loop for type in *unvariant-types*
          for enum = (enum-value
                      (interpret-call "QVariant" type))
          do (setf (aref new-map enum)
                   (format nil "to~a" type)))
    new-map))

(defun unvariant-non-core-map ()
  (with-cache ()
    ;;increase from 87 to 256 (23-may-2020) by Error: Array index 121 out of
    ;;bounds for #<SIMPLE-VECTOR 87> . in Qt5
    (let ((new-map (make-array 256))) 
      (loop for type in *unvariant-non-core-types*
            for enum = (enum-value
                        (interpret-call "QVariant" type))
            do (setf (aref new-map enum)
                     (find-qclass (format nil "Q~a" type))))
      new-map)))

(defun qvariant (value)
  ;; Memory managment of QVariants is unclear,
  ;; in some cases it can be deleted automatically, while not in others.
  ;; Disable caching, otherwise they will be stuck in the cache forever.
  (let ((*inhibit-caching* t))
    (etypecase value
      (string (#_new QVariant :|const QString&| value))
      (integer (#_new QVariant :|int| value))
      ((or single-float double-float) (#_new QVariant :|double| value))
      (boolean (#_new QVariant :|bool| value))
      (qobject
       (iter (for (code . type) in (variant-map))
         (when (qtypep value type)
           (return (#_new QVariant code (qobject-pointer value))))
         (finally (return value)))))))

(defun %unvariant (unvariant-map variant type)
  (let ((function (aref unvariant-map type)))
    (when (stringp function)
      (setf function
            (compile nil `(lambda (x)
                            (optimized-call nil x ,function)))
            (aref unvariant-map type) function))
    (funcall function variant)))

(defun unvariant (variant &optional (type (find-qtype "QVariant")))
  (let* ((qobject (%qobject (qtype-class type) variant))
         (code (enum-value (#_type qobject)))
         (unvariant-map (unvariant-map)))
    (if (array-in-bounds-p unvariant-map code)
        (%unvariant unvariant-map qobject code)
        (let* ((unvariant-map (unvariant-non-core-map))
               (class (and (array-in-bounds-p unvariant-map code)
                           (aref unvariant-map code))))
          (if class
              (%qobject class  (#_constData qobject))
              qobject)))))
