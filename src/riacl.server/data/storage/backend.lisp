(in-package :data.storage)

(defgeneric start-backend (name &rest args)
  (:documentation "Create a new backend instance."))

(defgeneric stop-backend (backend)
  (:documentation "Disconnect from the backend."))

(defgeneric put-value (backend key value)
  (:documentation "Put a value in the storage. The backend implementation has to make sure that access is porperly synchronized."))

(defmethod put-value (backend (key serialization:key) (value serialization:value))
  (error "Method not implemented."))

(defgeneric get-value (backend key)
  (:documentation "Get a value from the storage. The backend implementation has to make sure that access is porperly synchronized."))

(defmethod get-value (backend (key serialization:key))
  (error "Method not implemented."))

(defgeneric rem-value (backend key)
  (:documentation "Delete a value from the storage. The backend implementation has to make sure that access is porperly synchronized."))

(defmethod rem-value (backend (key serialization:key))
  (error "Method not implemented."))
