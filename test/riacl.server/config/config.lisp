(in-package :riacl.server.tests)

(define-test config-tests)

(define-test as-boolean-works :parent config-tests
  (true (config::as-boolean "yes"))
  (true (config::as-boolean "1"))
  (true (config::as-boolean "true"))
  (false (config::as-boolean "false")))
