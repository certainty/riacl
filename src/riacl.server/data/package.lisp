(in-package :cl-user)

(defpackage #:riacl.server.data.api
  (:use :cl)
  (:nicknames :data.api)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export #:start-api #:stop-api #:data-api-handler))
