(in-package :riacl.server.tests.config)

(define-test as-boolean-works ()
  (assert-true (config::as-boolean "yes"))
  (assert-true (config::as-boolean "1"))
  (assert-true (config::as-boolean "true"))
  (assert-false (config::as-boolean "false")))
