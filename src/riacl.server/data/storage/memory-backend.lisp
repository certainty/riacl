(in-package :data.storage)

(defvar *channel* (make-instance 'chanl:bounded-channel :size 1000))

(defclass in-memory-backend ()
  ((thread-handle
    :initform nil
    :accessor thread-handle
    :initarg :thread-handle
    :documentation "Thread handle for the in-memory backend")))

(defmethod start-backend ((name (eql :memory)) &rest args)
  (declare (ignore args))
  (log:info "Starting in-memory backend")
  (let ((handle (chanl:pcall #'handle-backend-messages
                             :name "in-memory-backend")))
    (make-instance 'in-memory-backend
                   :thread-handle handle))))

(defmethod stop-backend ((backend in-memory-backend))
  (log:info "Stopping in-memory backend")
  (with-slots (thread-handle) backend
    (chanl:send *channel* :stop)
    (bt2:join-thread thread-handle)))

(defun handle-backend-messages ()
  (let ((channel *channel*))
    (loop
      (let ((message (chanl:recv channel)))
        (log:info "Received message: ~a" message)
        (chanl:send channel :ok)))))
