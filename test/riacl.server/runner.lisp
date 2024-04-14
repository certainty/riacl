(in-package #:riacl.server.tests)

(defun run-suites ()
  (push :test *features*)
  (if (uiop:getenvp "CI_ENV")
      (progn
        (defvar cl-user::*exit-on-test-failures* t)
        (parachute:test :riacl.server.tests :report 'plain))
      (parachute:test :riacl.server.tests :report 'plain)))
