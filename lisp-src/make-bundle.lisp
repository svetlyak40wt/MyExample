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
  (unless (find #\/ dep)
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
   (get-all-deps '(:ningle :clack :clack-handler-toot))
   
   ;; '(ningle

   ;;   alexandria
   ;;   babel
   ;;   cffi
   ;;   circular-streams
   ;;   cl-annot
   ;;   cl-ppcre
   ;;   cl-syntax
   ;;   cl-syntax-annot
   ;;   cl-utilities
   ;;   fast-http
   ;;   fast-io
   ;;   flexi-streams
   ;;   http-body
   ;;   jonathan
   ;;   lack-component
   ;;   lack-request
   ;;   lack-response
   ;;   local-time
   ;;   map-set
   ;;   myway
   ;;   named-readtables
   ;;   proc-parse
   ;;   quri
   ;;   smart-buffer
   ;;   split-sequence
   ;;   static-vectors
   ;;   trivial-features
   ;;   trivial-gray-streams
   ;;   trivial-types
   ;;   uiop
   ;;   xsubseq

   ;;   clack-handler-toot
     
   ;;   alexandria
   ;;   babel
   ;;   bordeaux-threads
   ;;   cffi
   ;;   chunga
   ;;   cl+ssl
   ;;   cl-base64
   ;;   cl-fad
   ;;   cl-ppcre
   ;;   flexi-streams
   ;;   md5
   ;;   puri
   ;;   split-sequence
   ;;   toot
   ;;   trivial-backtrace
   ;;   trivial-features
   ;;   trivial-garbage
   ;;   trivial-gray-streams
   ;;   uiop
   ;;   usocket)
   :to "bundle/"))
