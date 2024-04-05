(in-package :riacl.server.cluster.identifier)

(s:defconst +riacl-prefix+ "urn:x-riacl" "The URN NID for riacl")

(defparameter *identifier-kinds* (list) "Associates a keyword with each kind of identifier")

(defun known-kind-p (kind)
  (member kind *identifier-kinds* :test #'string-equal))

(deftype kind () '(and keyword (satisfies known-kind-p)))

(-> is-riacl-urn-p (string) (values boolean &optional))
(defun is-riacl-urn-p (maybe-urn)
  (and (stringp maybe-urn)
       (str:starts-with-p +riacl-prefix+ maybe-urn)))

(deftype identifier () '(and string (satisfies is-riacl-urn-p)))

(defun identifier= (id1 id2)
  (string-equal id1 id2))

(defun identifier-sxhash (id)
  (sxhash id))

(sb-ext:define-hash-table-test identifier= identifier-sxhash)

(defun identifierp (id)
  (typep id 'identifier))

(-> kind-of (identifier) kind)
(defun kind-of (id)
  (let* ((uri (quri:uri id))
         (nss (quri:urn-nss uri)))
    (destructuring-bind (nid &rest _) (str:split ":" nss)
      (declare (ignore _))
      (if (member nid *identifier-kinds* :test #'string-equal)
          (string->keyword nid)
          (error "Unknown identifier kind: ~a" nid)))))

(-> make-identifier (kind list) string)
(defun make-identifier (kind segments)
  (let ((path (format nil "~{~a~^:~}" segments)))
    (string-downcase (format nil "~a:~a:~a" +riacl-prefix+ kind path))))

(defmacro define-identifier (kind constructor (&rest types-and-names) &optional docstring)
  (unless (keywordp kind)
    (error "Kind must be a keyword"))
  (let* ((constructor-params (mapcar #'car types-and-names))
         (constructor-param-types (mapcar #'cadr types-and-names))
         (kind-str (string-upcase (string kind)))
         (kind-predicate (string->symbol kind-str "-p"))
         (id-arg (gensym)))
    `(progn
       (setf *identifier-kinds* (remove-duplicates (cons ,kind-str *identifier-kinds*)))
       (-> ,constructor (,@constructor-param-types) identifier)
       (defun ,constructor (,@constructor-params)
         ,@(when docstring (list docstring))
         (make-identifier ,kind (list ,@constructor-params)))
       (defun ,kind-predicate (,id-arg)
         (and (identifierp ,id-arg)
              (eq (kind-of ,id-arg) ,kind))))))

(define-identifier :cluster cluster-name ((name string)) "Create a cluster identifier from the `name`.")

(define-identifier :node node-name ((network-address integer) (given-name string))
                   "Create a node-name from the `network-address` and `given-name`'.
The `network-address` is the IP address of the node.
The `given-name` is the name of the node as given by the user.")

(define-identifier :vnode vnode-name ((network-address integer) (given-name string) (index integer))
                   "Create a vnode identifier from the `network-address`, `node`, and `index`.
The `network-address` is the IP address of the node.
The `given-name` is the node identifier.
The `index` is the index of the vnode on the node.")

(define-identifier :bucket bucket-name ((namespace string) (name string))
                   "Create a bucket identifier from the `namespace` and `name`.
The `namespace` is the namespace of the bucket.
The `name` is the name of the bucket.")

(define-identifier :key object-key ((namespace string) (bucket-name string) (key string))
                   "Create a key identifier from the `namespace`, `bucket-name`, and `key`.
The `namespace` is the namespace of the key.
The `bucket-name` is the name of the bucket.
The `key` is the key of the object in the bucket.")
