(load "lisp-src/bundle/bundle.lisp")

(asdf:load-system :ningle)
(asdf:load-system :clack)
(asdf:load-system :clack-handler-toot)


(compile-file "lisp-src/my-example-user.lisp" :load :delete :output-file :temp)

(deliver-to-android-project nil "./" 5)

(quit)
