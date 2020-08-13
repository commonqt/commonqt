(in-package :qt)

;;; marshalling

(defmarshal (value (:|QStringList| :|const QStringList&|) :around cont :type list)
  (let ((qstringlist (sw_qstringlist_new)))
    (unwind-protect
         (progn
           (dolist (str value)
             (let ((char* (cffi:foreign-string-alloc str :encoding :utf-8)))
               (unwind-protect
                    (sw_qstringlist_append qstringlist char*)
                 (cffi:foreign-free char*))))
           (funcall cont qstringlist))
      (sw_qstringlist_delete qstringlist))))

(defmarshal (value (:|QList<int>| :|const QList<int>&|) :around cont :type list)
  (let ((qlist (sw_qlist_int_new)))
    (unwind-protect
         (progn
           (dolist (v value)
             (cffi:with-foreign-object (vptr :int)
               (setf (cffi:mem-ref vptr :int) v)
               (sw_qlist_int_append qlist vptr)))
           (funcall cont qlist))
      (sw_qlist_int_delete qlist))))

(defmacro define-object-ptr-list-marshaller (type-name)
  (let ((type-1 (alexandria:make-keyword (format nil "QList<~A*>" type-name)))
        (type-2 (alexandria:make-keyword (format nil "const QList<~A*>&" type-name))))
    `(defmarshal (value (,type-1 ,type-2) :around cont :type list)
       (let ((qlist (sw_qlist_void_new))
             (element-class (with-cache () (find-qclass ,type-name))))
         (unwind-protect
              (progn
                (dolist (v value)
                  (unless (and (typep v 'abstract-qobject)
                               (qtypep v element-class))
                    (error "Cannot marshal list element ~s as ~s" v ,type-name))
                  (sw_qlist_void_append qlist (qobject-pointer v)))
                (funcall cont qlist))
           (sw_qlist_void_delete qlist))))))

(define-object-ptr-list-marshaller "QObject")
(define-object-ptr-list-marshaller "QStandardItem")
(define-object-ptr-list-marshaller "QAction")
(define-object-ptr-list-marshaller "QAbstractState")
(define-object-ptr-list-marshaller "QGraphicsTransform")

(defmarshal (value (:|QList<QByteArray>| :|const QList<QByteArray>&|) :around cont :type list)
  (let ((qlist (sw_qlist_qbytearray_new)))
    (unwind-protect
         (progn
           (dolist (v value)
             (let ((vptr (sw_make_qbytearray v)))
               (unwind-protect
                    (sw_qlist_qbytearray_append qlist vptr)
                 (sw_delete_qbytearray vptr))))
           (funcall cont qlist))
      (sw_qlist_qbytearray_delete qlist))))

(defmarshal (value (:|QList<QVariant>| :|const QList<QVariant>&|) :around cont :type list)
  (let ((qlist (sw_qlist_qvariant_new)))
    (unwind-protect
         (progn
           (dolist (v value)
             (sw_qlist_qvariant_append qlist (qobject-pointer (qvariant v))))
           (funcall cont qlist))
      (sw_qlist_qvariant_delete qlist))))

(defmacro define-copyable-object-list-marshaller (type-name)
  (let ((type-1 (alexandria:make-keyword (format nil "QList<~A>" type-name)))
        (type-2 (alexandria:make-keyword (format nil "const QList<~A>&" type-name)))
        (new-func (qlist-function-name type-name 'new))
        (append-func (qlist-function-name type-name 'append))
        (delete-func (qlist-function-name type-name 'delete)))
    `(defmarshal (value (,type-1 ,type-2) :around cont :type list)
       (let ((qlist (,new-func))
             (element-class (with-cache () (find-qclass ,type-name))))
         (unwind-protect
              (progn
                (dolist (v value)
                  (unless (and (typep v 'abstract-qobject)
                               (qtypep v element-class))
                    (error "Cannot marshal list element ~s as ~s" v ,type-name))
                  (,append-func qlist (qobject-pointer v)))
                (funcall cont qlist))
           (,delete-func qlist))))))

(define-copyable-object-list-marshaller "QModelIndex")
(define-copyable-object-list-marshaller "QKeySequence")
(define-copyable-object-list-marshaller "QTextEdit::ExtraSelection")

;;; unmarshalling

(def-unmarshal (value "QStringList" type)
  (iter (for i below (sw_qstringlist_size value))
        (collect (convert-qstring-data (sw_qstringlist_at value i)))))

(def-unmarshal (value "QList<int>" type)
  (iter (for i below (sw_qlist_int_size value))
        (collect (cffi:mem-ref (sw_qlist_int_at value i) :int))))

(defmacro define-object-ptr-list-unmarshaller (type-name)
  (let ((list-type (format nil "QList<~A*>" type-name)))
    `(def-unmarshal (value ,list-type type)
       (iter (for i below (sw_qlist_void_size value))
             (collect (%qobject (with-cache () (find-qclass ,type-name))
                                (sw_qlist_void_at value i)))))))

(defmacro define-object-list-unmarshaller (type-name)
  (let ((list-type (format nil "QList<~A>" type-name)))
    `(def-unmarshal (value ,list-type type)
       (iter (for i below (sw_qlist_void_size value))
         (collect (%qobject (with-cache () (find-qclass ,type-name))
                            (sw_qlist_void_at value i)))))))

(def-unmarshal (value "QList<QByteArray>" type)
  (iter (for i below (sw_qlist_qbytearray_size value))
        (collect (%qobject (find-qclass "QByteArray") (sw_qlist_qbytearray_at value i)))))

(define-object-ptr-list-unmarshaller "QObject")
(define-object-ptr-list-unmarshaller "QWidget")
(define-object-ptr-list-unmarshaller "QStandardItem")
(define-object-ptr-list-unmarshaller "QListWidgetItem")
(define-object-ptr-list-unmarshaller "QTreeWidgetItem")
(define-object-ptr-list-unmarshaller "QTableWidgetItem")
(define-object-ptr-list-unmarshaller "QGraphicsItem")
(define-object-ptr-list-unmarshaller "QGraphicsView")
(define-object-ptr-list-unmarshaller "QGraphicsWidget")
(define-object-ptr-list-unmarshaller "QAction")
(define-object-ptr-list-unmarshaller "QAbstractButton")
(define-object-ptr-list-unmarshaller "QTextFrame")
(define-object-ptr-list-unmarshaller "QAbstractState")
(define-object-ptr-list-unmarshaller "QUndoStack")
(define-object-ptr-list-unmarshaller "QMdiSubWindow")
(define-object-ptr-list-unmarshaller "QGraphicsTransform")

(define-object-list-unmarshaller "QPrinterInfo")
(define-object-list-unmarshaller "QTextEdit::ExtraSelection")


(defmacro define-copyable-object-list-unmarshaller (type-name)
  (let ((list-type (format nil "QList<~A>" type-name))
        (size-func (qlist-function-name type-name 'size))
        (at-func (qlist-function-name type-name 'at)))
    `(def-unmarshal (value ,list-type type)
       (iter (for i below (,size-func value))
         ;; clone objects so that the pointers don't become invalid
         ;; when the list is destroyed or they're removed from it
         (collect (optimized-new
                   ,type-name
                   (%qobject (find-qclass ,type-name) (,at-func value i))))))))

(define-copyable-object-list-unmarshaller "QModelIndex")
(define-copyable-object-list-unmarshaller "QKeySequence")

(def-unmarshal (value "QList<QVariant>" type)
  (iter (for i below (sw_qlist_qvariant_size value))
        (collect (unvariant (sw_qlist_qvariant_at value i)))))

(def-unmarshal (value ("QList<QPrinter::PageSize>"
                       "QList<QPrinter::PaperSize>") type)
  (iter (for i below (sw_qlist_papersize_size value))
    (collect (enum (cffi:mem-ref (sw_qlist_papersize_at value i) :int)
                   :|QPrinter::PaperSize|))))
