(in-package :cl-user)


(defun expand-dependencies (system-name)
  (remove nil
          (list* (let ((system-name
                         (etypecase system-name
                           (list (ecase (first system-name)
                                   (:version (second system-name))
                                   ;; just skip featured deps for now
                                   ;; most cases are special dependencies for :sbcl
                                   (:feature nil)))
                           (symbol (string-downcase (symbol-name system-name)))
                           (string system-name))))
                   (when system-name
                     (asdf:primary-system-name system-name)))
                 (typecase system-name
                   ((or string symbol)
                    (let ((system (asdf/system:find-system system-name)))
                      (loop for dep in (asdf/system:system-depends-on system)
                            appending (expand-dependencies dep))))))))


(defun is-good (dep)
  (unless (and (> (length dep)
                  3)
               (string-equal (subseq dep 0 3)
                             "sb-"))
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
  (let ((deps '("todo")))
    (pushnew "./lisp-src/" asdf:*central-registry*
             :test 'equal)
    (ql:quickload deps)
    (ql:bundle-systems
     (remove-if (lambda (item)
                  (member item deps :test #'string-equal))
                (get-all-deps deps))
     :to "lisp-src/bundle/")))
