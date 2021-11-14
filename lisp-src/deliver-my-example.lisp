(load "lisp-src/bundle/bundle.lisp")

(push :dexador-no-ssl
      *features*)

(push "lisp-src/" asdf:*central-registry*)

(defun force-utf-8-file-encoding (pathname ef-spec buffer length)
  (declare (ignore pathname buffer length))
  ;; (system:merge-ef-specs ef-spec :utf-8)
  (system:merge-ef-specs ef-spec '(:utf-8 :eol-style :lf)))

(set-default-character-element-type 'character)

;; This will change how encoding for all loaded lisp
;; files is detected.
(setf system:*file-encoding-detection-algorithm*
      '(force-utf-8-file-encoding))

(setf stream::*default-external-format* '(:utf-8 :eol-style :lf))


(asdf:load-system :todo)

;; (asdf:load-system :ningle)
;; (asdf:load-system :clack)
;; (asdf:load-system :weblocks)
;; (asdf:load-system :clack-handler-hunchentoot)

(compile-file "lisp-src/my-example-user.lisp" :load :delete :output-file :temp)


(deliver-to-android-project nil "./"
                            ;; delivery level
                            0
                            ;; With this level app does not work :(
                            ;; 5
                            ;; Without this lack-middleware-backtrace can't be loaded
                            :keep-eval t)

(quit)
