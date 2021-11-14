(in-package "CL-USER")

(export 'get-url-to-display)
(export 'start-server)

(declaim (notinline get-url-to-display
                    start-server))


;; (defvar *app* (make-instance 'ningle:app))


;; (setf (ningle:route *app* "/")
;;       (spinneret:with-html-string
;;         (:html
;;          (:body :style "margin: 0px"
;;           (:div :style "width: 100%; height: 200px; background: #F55;"
;;                 (:div :style "background: white; width: 100%; height: 100%; padding: 5px; margin-left: 5px; margin-top: 5px;"
;;                  (:p "Hello from Hunchentoot!")
;;                  (:dl
;;                   (:dt "Lisp implementation")
;;                   (:dd (lisp-implementation-type))
;;                   (:dt "Lisp version")
;;                   (:dd (lisp-implementation-version)))))))))


(defun start-server (&key (port 10080))
  (clack:clackup *app*
                 :port port :server :hunchentoot))


(defun get-url-to-display ()
  (handler-bind ((error (lambda (c)
                          (return-from get-url-to-display
                            (format nil "Traceback: ~A" c))) ))
    (todo:start-server)))


(deliver-keep-symbols 'get-url-to-display 'start-server)
