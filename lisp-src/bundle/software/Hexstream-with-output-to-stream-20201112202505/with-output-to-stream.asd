(asdf:defsystem #:with-output-to-stream

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "Provides a simple way of directing output to a stream according to the concise and intuitive semantics of FORMAT's stream argument."

  :depends-on ()

  :version "1.0"
  :serial cl:t
  :components ((:file "package")
	       (:file "main"))

  :in-order-to ((asdf:test-op (asdf:test-op #:with-output-to-stream_tests))))
