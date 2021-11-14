(in-package #:with-output-to-stream)

(defun %call-with-output-to-stream (function stream element-type)
  (if stream
      (progn
        (if (stringp stream)
            (with-output-to-string (var stream)
              (funcall function var))
            (funcall function stream))
        nil)
      (with-output-to-string (var nil :element-type element-type)
        (funcall function var))))

(defmacro with-output-to-stream ((var &optional stream &key (element-type 'character)) &body body)
  `(%call-with-output-to-stream (lambda (,var)
                                  ,@body)
                                ,(if (eq stream t)
                                     '*standard-output*
                                     `(if (eq ,stream t)
                                          *standard-output*
                                          ,stream))
                                ',element-type))
