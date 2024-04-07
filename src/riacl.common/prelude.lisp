(in-package #:riacl.common.prelude)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (-> keyword->string (keyword) string)
  (defun keyword->string (kw)
    (string-downcase (string kw)))

  (-> string->keyword (string) keyword)
  (defun string->keyword (s)
    (multiple-value-bind (n _) (intern (string-upcase s) :keyword)
      (declare (ignore _))
      n))

  (defun string->symbol (name &rest parts)
    (intern (string-upcase (apply #'concatenate 'string name parts)))))

(defgeneric to-plist (obj)
  (:documentation "Create a representation of an object as a property list.
This needs to be a stable representation, as it can be used for serialization or testing"))
