(in-package :riacl.server.data)

(defclass namespace ()
  ((type :reader namespace-type
         :initarg :type
         :initform (error "type not initialized")
         :documentation "The type of the namespace")
   (bucket :reader namespace-bucket
           :initarg :bucket
           :initform (error "bucket not initialized")
           :documentation "The bucket of the namespace")))

(defun namespace-equal (n1 n2)
  (and
   (equal (namespace-type n1) (namespace-type n2))
   (equal (namespace-bucket n1) (namespace-bucket n2))))

(defclass key ()
  ((namespace
    :reader key-namespace
    :initarg :namespace
    :initform (error "namespace not initialized")
    :documentation "The namespace of the key"
    :type namespace)
   (data
    :reader key-data
    :initarg :key
    :initform (error "key not initialized")
    :documentation "The object key")))

(make-hash-table :test 'equal)

(defun key-equal (k1 k2)
  (and
   (namespace-equal (key-namespace k1) (key-namespace k2))
   (equal (key-data k1) (key-data k2))))

(defun key-sxhash (k)
  (with-slots (namespace data) k
    (sxhash (list (namespace-type namespace) (namespace-bucket namespace) data))))

(sb-ext:define-hash-table-test key-equal key-sxhash)

(defclass metadata ()
  ((vector-clock
    :reader meta-vector-clock
    :initarg :vector-clock
    :initform (error "vector-clock not initialized")
    :type vector-clock:vector-clock
    :documentation "The vector clock of the object")
   (nval
    :reader meta-nval
    :initarg :nval
    :initform 0
    :type integer
    :documentation "The number of values in the set")
   (content-type
    :reader meta-content-type
    :initarg :content-type
    :initform (error "content-type not initialized")
    :documentation "The content type of the object")))

(defun make-metadata (vector-clock content-type)
  (make-instance 'metadata :vector-clock vector-clock :nval nval :content-type content-type))

(defclass value ()
  ((meta
    :reader value-meta
    :initarg :meta
    :initform (error "meta not initialized")
    :documentation "The metadata of the value")))

(defclass generic-value (value)
  ((data
    :initarg :data
    :initform nil
    :type (or null (vector (unsigned-byte 8)))
    :documentation "A generic value which holds any kind of data in binary form")))

(defclass tombstone-value (value) ()
  (:documentation "A tombstone value"))

(defun make-tombstone-value ()
  (make-instance
   'tombstone-value
   :meta
   (make-metadata (vector-clock:make-vector-clock) "application/octet-stream")))

(defclass counter-value (value)
  ((counter
    :initarg :counter
    :initform 0
    :type integer
    :documentation "A counter value")))

(defclass flag-value (value)
  ((flag
    :initarg :flag
    :initform nil
    :type boolean
    :documentation "A flag value")))

(defclass set-value (value)
  ((set
    :initarg :set
    :initform (make-hash-table :test 'equal)
    :documentation "A set value")))

(defclass map-value (value)
  ((map
    :initarg :map
    :initform (make-hash-table :test 'equal)
    :documentation "A map value")))
