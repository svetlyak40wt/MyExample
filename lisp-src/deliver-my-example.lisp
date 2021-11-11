(compile-file "lisp-src/my-example-user.lisp" :load :delete :output-file :temp)

(deliver-to-android-project nil "./" 5)

(quit)
