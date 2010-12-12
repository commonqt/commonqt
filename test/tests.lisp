(in-package :qt-tests)

(named-readtables:in-readtable :qt)

(defmacro with-qapp (&body body)
  `(let ((qapp (ensure-qapplication)))
     (declare (ignorable qapp))
     ,@body))

;; CommonQt reader macro works by defining some new macros.
;; This is ok for ordinary compilation but with RT test
;; cases the macroexpansion is done when tests are run,
;; and the newly defined macros don't make it into fasls,
;; so loading compiled tests fails. We have to put the
;; test body into separate defun, sacrificing runtime
;; macro expanding.

(defmacro deftest/qt (name form &rest values)
  (alexandria:with-gensyms (func-name)
    `(progn
       (defun ,func-name () (with-qapp ,form))
       (deftest ,name (,func-name) ,@values))))

(let ((bad (cons nil nil)))
  (defun remarshal (value type &optional with-const-p)
    (cffi:with-foreign-object (stack-item 'qt::|union StackItem|)
      (let ((result bad)
            (<type> (or (qt::find-qtype type)
                        (error "no such type ~s" type))))
        (assert (qt::can-marshal-p value <type>) ()
                "cannot marshal ~s as ~s" value type)
        (qt::marshal value <type>
                     stack-item #'(lambda ()
                                    (setf result
                                          (qt::unmarshal <type> stack-item))))
        (assert (not (eq bad result)) () "marshalling continuation not invoked")
        (when with-const-p
          (let* ((const-type (format nil "const ~A&" type))
                 (<const-type> (qt::find-qtype const-type)))
            (assert (qt::can-marshal-p value <const-type>) ()
                    "cannot marshal ~s as ~s" value const-type)
            (qt::marshal value <const-type>
                         stack-item
                         #'(lambda ()
                             (let ((v (qt::unmarshal <type> stack-item)))
                               (assert (equal result v)
                                       () "remarshal: got ~s instead of ~s when marshalling using const ~A&"
                                       v result type))))))
        result))))

(defmacro define-marshalling-test (name type with-const-p &rest values)
  `(deftest/qt ,name
       (values ,@(iter (for val in values)
                       (collect `(remarshal ',val ,type ,with-const-p))))
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

(deftest/qt test-qobjectlist-marshalling
    (let ((a (#_new QObject))
          (b (#_new QPushButton "Def"))
          (c (#_new QLabel "zzz")))
      (#_setObjectName a "Abc")
      (flet ((extract (list)
               (list
                (#_objectName (first list))
                (#_text (second list))
                (#_text (third list)))))
        (extract (remarshal (list a b c) "QList<QObject*>" t))))
  ("Abc" "Def" "zzz"))

(deftest/qt test-object-children
    (let* ((a (#_new QObject))
           (b (#_new QObject a))
           (c (#_new QObject a)))
      (set-difference (list b c) (#_children a)))
  nil)

(deftest/qt test-item-model-stuff-marshalling
    (let ((model (#_new QStandardItemModel)))
      (#_appendRow model (list (#_new QStandardItem "01")
                               (#_new QStandardItem "bca")))
      (#_appendRow model (list (#_new QStandardItem "02")
                               (#_new QStandardItem "abc")))
      (#_appendRow model (list (#_new QStandardItem "03")
                               (#_new QStandardItem "bcq")))
      (values
        (iter (for item in (remarshal (list (#_new QStandardItem "zz")
                                            (#_new QStandardItem "rr"))
                                      "QList<QStandardItem*>"))
              (collect (#_text item)))
        (iter (for i from 0 to 2)
              (collect (cons (#_data model (#_index model i 0))
                             (#_data model (#_index model i 1)))))
        (iter (for index in (#_match model (#_index model 0 1)
                                     (#_Qt::DisplayRole) "bc" -1))
              (collect (cons (#_row index) (#_column index))))
        (iter (for index in (remarshal (#_match model (#_index model 0 1)
                                                (#_Qt::DisplayRole) "bc" -1)
                                       "QList<QModelIndex>"))
              (collect (cons (#_row index) (#_column index))))))
  ("zz" "rr") (("01" . "bca") ("02" . "abc") ("03" . "bcq"))
  ((0 . 1) (2 . 1)) ((0 . 1) (2 . 1)))

(deftest/qt test-no-enum-confusion
    (let ((action (#_new QAction (null-qobject (find-qclass "QAction"))))
          (keys (list (#_new QKeySequence :|int| (#_Qt::Key_Backspace))
                      (#_new QKeySequence (#_QKeySequence::Back)))))
      (#_setShortcuts action keys)
      (iter (for shortcut in (#_shortcuts action))
            (collect (#_toString shortcut))))
  ("Backspace" "Alt+Left"))

;; TBD: deconstify types when looking for marshaller/unmarshaller, remove (macro-generated) duplicate marshaller definitions
