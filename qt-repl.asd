(defsystem :qt-repl
    :serial t
    :components ((:file "repl-integration"))
    :depends-on (:qt :bordeaux-threads))
