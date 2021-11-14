(cl:defpackage #:with-output-to-stream_tests
  (:use #:cl #:parachute)
  (:import-from #:with-output-to-stream #:with-output-to-stream))

(cl:in-package #:with-output-to-stream_tests)

(define-test "main"
  (let ((s "Test data."))
    (is string= s
        (with-output-to-string (stream)
          (is eq nil
              (with-output-to-stream (stream stream)
                (write-string s stream)))))
    (let ((nil-value nil))
      (is string= s
          (with-output-to-stream (stream)
            (write-string s stream)))
      (is string= s
          (with-output-to-stream (stream nil)
            (write-string s stream)))
      (is string= s
          (with-output-to-stream (stream nil-value)
            (write-string s stream))))
    (let ((t-value t))
      (is string= s
          (with-output-to-string (*standard-output*)
            (is eq nil
                (with-output-to-stream (stream t)
                  (write-string s stream)))))
      (is string= s
          (with-output-to-string (*standard-output*)
            (is eq nil
                (with-output-to-stream (stream t-value)
                  (write-string s stream))))))
    (is string= s
        (let ((string (make-array 16 :element-type 'character :fill-pointer 0)))
          (is eq nil
              (with-output-to-stream (stream string)
                (write-string s stream)))
          string))))
