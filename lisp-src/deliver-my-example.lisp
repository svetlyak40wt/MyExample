(load "lisp-src/bundle/bundle.lisp")

(push :dexador-no-ssl
      *features*)

(push "lisp-src/" asdf:*central-registry*)

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
