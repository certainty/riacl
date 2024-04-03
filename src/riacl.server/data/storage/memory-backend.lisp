(in-package :data.storage.memory)

(defvar *state* (make-hash-table :test 'data:key-equal :synchronized t))
(defvar *state-mutex* (sb-thread:make-mutex))

(defclass in-memory-backend () ())

(defmethod start-backend ((name (eql :memory)) &rest args)
  (declare (ignore args))
  (log:info "[Storage] starting in-memory backend")
  ((make-instance 'in-memory-backend)))

(defmethod stop-backend ((backend in-memory-backend))
  (log:info "[Storage] stopping in-memory backend"))

(defmethod put-value ((backend in-memory-backend) key value)
  (setf (gethash key *state*) value))

(defmethod get-value ((backend in-memory-backend) key)
  (gethash key *state*))

(defmethod delete-value ((backend in-memory-backend) key)
  (remhash key *state*))
