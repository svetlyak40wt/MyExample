(asdf:defsystem #:with-output-to-stream_tests

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "with-output-to-stream unit tests."

  :depends-on ("with-output-to-stream"
               "parachute")

  :serial cl:t
  :components ((:file "tests"))

  :perform (asdf:test-op (op c) (uiop:symbol-call '#:parachute '#:test '#:with-output-to-stream_tests)))
