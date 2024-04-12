(in-package #:riacl.server.cli)

(defun signal-stop-gracefully (signo)
  "Signal the server to stop gracefully."
  (declare (ignore signo))
  (log:info "[Server] Recieved stop signal. Stopping server.")
  (riacl.server:stop-server)
  (sb-ext:quit))

(defun signal-restart (signo)
  "Signal the server to restart."
  (declare (ignore signo))
  (log:info "[Server] Recieved restart signal. Restarting server")
  (riacl.server:stop-server)
  (riacl.server:start-server))

(defun main (&rest args)
  (declare (ignorable args))
  (trivial-signal:signal-handler-bind ((:term #'signal-stop-gracefully)
                                       (:int #'signal-stop-gracefully)
                                       (:quit #'signal-stop-gracefully)
                                       (:hup #'signal-restart)
                                       (:usr1 #'signal-stop-gracefully))
    (riacl.server:start-server)
    ;; wait for signals
    (loop (sleep 1))))
