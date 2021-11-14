(in-package :cl-user)


(defun expand-dependencies (system-name)
  (list* (etypecase system-name
           (symbol (string-downcase (symbol-name system-name)))
           (string system-name))
         (typecase system-name
           ((or string symbol)
            (let ((system (asdf/system:find-system system-name)))
              (loop for dep in (asdf/system:system-depends-on system)
                    appending (expand-dependencies dep)))))))


(defun is-good (dep)
  (unless (or (find #\/ dep)
              (and (> (length dep)
                      3)
                   (string-equal (subseq dep 0 3)
                                 "sb-")))
    t))


(defun get-all-deps (root-deps)
  (loop with all-deps = nil
        for root-dep in root-deps
        for deps = (expand-dependencies root-dep)
        do (loop for dep in deps
                 when (and (not (member dep all-deps
                                        :test #'string-equal))
                           (is-good dep))
                   do (push dep all-deps))
        finally (return (sort all-deps #'string<))))


(defun make-bundle ()
  (ql:bundle-systems
   (get-all-deps '(:ningle :clack
                   :clack-handler-toot
                   :clack-handler-hunchentoot))
   :to "bundle/"))
