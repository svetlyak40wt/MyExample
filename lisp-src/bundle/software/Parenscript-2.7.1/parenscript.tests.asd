;;;; -*- lisp -*-

(defsystem :parenscript.tests
  :license "BSD-3-Clause"
  :description "Unit tests for Parenscript"
  :components ((:module :tests
                        :serial t
                        :components ((:file "test-package")
                                     (:file "test")
                                     (:file "output-tests")
                                     (:file "package-system-tests")
                                     (:file "eval-tests"))))
  :depends-on (:parenscript :fiveam :cl-js))
