(in-package "CL-USER")

(export 'get-url-to-display)
(declaim (notinline get-url-to-display))

(defun get-url-to-display ()
   "https://svetlyak.ru")

(deliver-keep-symbols 'get-url-to-display)
