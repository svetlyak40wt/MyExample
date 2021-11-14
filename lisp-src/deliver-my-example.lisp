(load "lisp-src/bundle/bundle.lisp")

(push :dexador-no-ssl
      *features*)

(push "lisp-src/" asdf:*central-registry*)

(defun force-utf-8-file-encoding (pathname ef-spec buffer length)
  (declare (ignore pathname buffer length))
  ;; (system:merge-ef-specs ef-spec :utf-8)
  (system:merge-ef-specs ef-spec '(:utf-8 :eol-style :lf)))

(set-default-character-element-type 'character)

;; This will change how encoding for all loaded lisp
;; files is detected.
(setf system:*file-encoding-detection-algorithm*
      '(force-utf-8-file-encoding))

(setf stream::*default-external-format* '(:utf-8 :eol-style :lf))


(asdf:load-system :todo)

;; (asdf:load-system :ningle)
;; (asdf:load-system :clack)
;; (asdf:load-system :weblocks)
;; (asdf:load-system :clack-handler-hunchentoot)

(compile-file "lisp-src/my-example-user.lisp" :load :delete :output-file :temp)

;; To try the remote debugging, Uncomment the (require "remote-debugger-client") line below.
;; After the application starts, you can connect to it from an IDE using
;; dbg:ide-connect-remote-debugging. Note that this requires comm:OPEN-TCP-STREAM to work,
;; which means that you need to allow internet permission to the application by uncommenting
;; the INTERNET line in the AndroidManifest.xml file (You get "Permission denied (13)" error otherwise).
;;  The IDE machine needs also to be able to find
;; the device. A simple way to schieve that is to use the adb utility that comes with
;; the android SDK to map the port from the IDE machine to the device. The default
;; port is 21102, so you need:
;;     /Users/art/Library/Android/sdk/platform-tools/adb forward tcp:21102 tcp:21102
;;; after that,
;;     (dbg:ide-connect-remote-debugging "localhost" :open-a-listener t)
;; should work on the IDE machine.
(require "remote-debugger-client")

(defun my-initialization-function ()
  ;; Uncomment the (require "remote-debugger-client") above for this to work
  (lw:quit)
  (when (fboundp 'dbg:start-client-remote-debugging-server)
    (dbg:start-client-remote-debugging-server)))

(deliver-to-android-project 'my-initialization-function
                            "./"
                            ;; delivery level
                            0
                            ;; With this level app does not work :(
                            ;; 5
                            ;; Without this lack-middleware-backtrace can't be loaded
                            :keep-eval t
                            ;; For remote debugging
                            :keep-macros t
                            :packages-to-keep-externals '("TODO"
                                                          "CL-USER"
                                                          "COMMON-LISP"
                                                          "HCL"
                                                          "LISPWORKS"
                                                          "LW-JI"
                                                          "MP"))

(quit)
