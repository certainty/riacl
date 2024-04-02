(in-package #:riacl.server)

(defun setup-logging ()
  (log:config :file2)
  (log:config config:*log.level*))
