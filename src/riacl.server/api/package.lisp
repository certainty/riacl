(in-package :cl-user)

(defpackage #:riacl.server.api.foundation
  (:use :cl)
  (:nicknames :api.foundation :api)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:start-api
   #:stop-api
   #:api-endpoint
   #:api-handler
   #:api-name
   #:api-list-address
   #:make-api-endpoint
   #:add-route
   #:add-routes
   #:get-request-header
   #:register-encoder
   #:encode-content
   #:json-encoder
   #:msgpack-encoder
   #:respond-with
   #:respond-with-error))
