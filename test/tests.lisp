(in-package :qt-tests)

(named-readtables:in-readtable :qt)

(defmacro with-qapp (&body body)
  `(let ((qapp (ensure-qapplication)))
     (declare (ignorable qapp))
     ,@body))

(let ((bad (cons nil nil)))
  (defun remarshal (value type &optional with-const-p)
    (cffi:with-foreign-object (stack-item 'qt::|union StackItem|)
      (let ((result bad))
        (qt::marshal value (qt::find-qtype type)
                     stack-item #'(lambda ()
                                    (setf result
                                          (qt::unmarshal
                                           (qt::find-qtype type)
                                           stack-item))))
        (assert (not (eq bad result)) () "marshalling continuation not invoked")
        (when with-const-p
          (qt::marshal value (qt::find-qtype (format nil "const ~A&" type))
                       stack-item
                       #'(lambda ()
                           (let ((v (qt::unmarshal
                                     (qt::find-qtype type)
                                     stack-item)))
                             (assert (equal result v)
                                     () "remarshal: got ~s instead of ~s when marshalling using const ~A&"
                                     v result type)))))
        result))))

(defmacro define-marshalling-test (name type with-const-p &rest values)
  `(deftest ,name
       (with-qapp
         (values ,@(iter (for val in values)
                         (collect `(remarshal ',val ,type ,with-const-p)))))
     ,@values))

(define-marshalling-test test-qbytearray-marshalling
    "QByteArray" t
  "" "abc" "qwerty uiop" #.(make-string 3 :initial-element (code-char 1093)))

(define-marshalling-test test-qvariant-marshalling
    "QVariant" t
  "" 123 "zzz" #.(make-string 3 :initial-element (code-char 1093)))

(define-marshalling-test test-qstring-marshalling
    "QString" t
  "" "abc" "qwerty uiop" #.(make-string 3 :initial-element (code-char 1093)))

(define-marshalling-test test-qstringlist-marshalling
    "QStringList" t
  () ("abc") ("" "abcd" "qqqq" "rrr") ("abc" "Def" "ghi"))

(define-marshalling-test test-qlistint-marshalling
    "QList<int>" t
  () (42) (#x7fffffff 12345 678) (11 12))

(define-marshalling-test test-qlistbytearray-marshalling
    "QList<QByteArray>" t
  () ("abc") ("" "abcd" "qqqq" "rrr") ("abc" "Def" "ghi"))

(define-marshalling-test test-qlistqvariant-marshalling
    "QList<QVariant>" t
  () ("abc") ("" 123 "zzz" 456))

(deftest test-qobjectlist-marshalling
    (progn
      (ensure-qapplication)
      (let ((a (#_new QObject))
            (b (#_new QPushButton "Def"))
            (c (#_new QLabel "zzz")))
        (#_setObjectName a "Abc")
        (flet ((extract (list)
                 (list
                  (#_objectName (first list))
                  (#_text (second list))
                  (#_text (third list)))))
          (extract (remarshal (list a b c) "QList<QObject*>" t)))))
  ("Abc" "Def" "zzz"))
