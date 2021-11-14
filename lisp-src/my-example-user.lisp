(in-package "CL-USER")

(export 'get-url-to-display)
(declaim (notinline get-url-to-display))

(defun get-url-to-display ()
  "https://svetlyak.ru")


(defvar *app* (make-instance 'ningle:app))


(setf (ningle:route *app* "/")
      "Welcome to ningle!")


(export 'start-server)
(declaim (notinline start-server))

(defun start-server (&key (port 10080))
  (clack:clackup *app* :port port :server :toot))


(deliver-keep-symbols 'get-url-to-display 'start-server)
