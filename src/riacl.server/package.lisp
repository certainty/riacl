(in-package :cl-user)

(defpackage #:riacl.server
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export #:start-server #:stop-server))
