(defsystem "todo"
  :class :package-inferred-system
  :depends-on ("todo/todo"))


(asdf:register-system-packages "clack-handler-hunchentoot" '("CLACK.HANDLER.HUNCHENTOOT"))
