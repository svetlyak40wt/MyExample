(in-package :cl-user)

(defun make-bundle ()
  (ql:bundle-systems
   '(ningle

     alexandria
     babel
     cffi
     circular-streams
     cl-annot
     cl-ppcre
     cl-syntax
     cl-syntax-annot
     cl-utilities
     fast-http
     fast-io
     flexi-streams
     http-body
     jonathan
     lack-component
     lack-request
     lack-response
     local-time
     map-set
     myway
     named-readtables
     proc-parse
     quri
     smart-buffer
     split-sequence
     static-vectors
     trivial-features
     trivial-gray-streams
     trivial-types
     uiop
     xsubseq

     clack-handler-toot
     
     alexandria
     babel
     bordeaux-threads
     cffi
     chunga
     cl+ssl
     cl-base64
     cl-fad
     cl-ppcre
     flexi-streams
     md5
     puri
     split-sequence
     toot
     trivial-backtrace
     trivial-features
     trivial-garbage
     trivial-gray-streams
     uiop
     usocket)
   :to "bundle/"))
