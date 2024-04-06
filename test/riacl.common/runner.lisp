(in-package #:riacl.common.tests)

(defmacro with-env-specific-setup (&body body)
  (let ((db (gensym)))
    `(if (uiop:getenvp "CI_ENV")
         (let ((*debugger-hook*))
           (let ((,db (with-summary () ,@body)))
             (unless (and (null (lisp-unit2::failed-assertions ,db))
                          (null (errors ,db)))
               (uiop:quit 1))))
         (with-summary () ,@body))))

(defun run-suites ()
  (push :test *features*)
  (with-env-specific-setup
    (run-tests
     :name "riacl.common.tests"
     :package '(#:riacl.common.tests.network))))
