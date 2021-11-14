(in-package "CL-USER")

(export 'get-url-to-display)
(export 'start-server)

(declaim (notinline get-url-to-display
                    start-server))


(defvar *app* (make-instance 'ningle:app))


(setf (ningle:route *app* "/")
      "<html>
  <body>
    <div style=\"width: 300px; height: 200px; background: #F55;\">
      Hello World!
    </div>
  </body>
</html>
")

(defun start-server (&key (port 10080))
  (clack:clackup *app*
                 :port port :server :hunchentoot))


(defun get-url-to-display ()
  (handler-bind ((error (lambda (c)
                          (return-from get-url-to-display
                            (format nil "Traceback: ~A" c))) ))
    (start-server)
    (sleep 10)
    (values "http://127.0.0.1:10080/")
    ;; (values "https://svetlyak.ru/")
    ))


(deliver-keep-symbols 'get-url-to-display 'start-server)
