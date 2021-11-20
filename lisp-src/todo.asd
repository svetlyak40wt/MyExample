(defsystem "todo"
  :class :package-inferred-system
  :depends-on ("todo/todo"
               "slynk/mrepl"
               "slynk/arglists"))


(asdf:register-system-packages "clack-handler-hunchentoot" '("CLACK.HANDLER.HUNCHENTOOT"))

