(in-package #:riacl.common.tests)

(defun run-suites ()
  (push :test *features*)
  (if (uiop:getenvp "CI_ENV")
      (progn
        (defvar cl-user::*exit-on-test-failures* t)
        (parachute:test :riacl.common.tests :report 'plain))
      (parachute:test :riacl.common.tests :report 'interactive)))
