(defsystem "clidle-server"
  :version "0.1.0"
  :author "Momozor"
  :license "AGPL-3.0"
  :depends-on ("swank" 
               "com.google.base")
  :components ((:module "src"
                        :components
                        ((:file "main"))))
  :build-operation "program-op" ;; leave as is
  :build-pathname "clidle-server"
  :entry-point "clidle-server:main"
  :description "Server side for CLIDLE")

(defsystem "clidle-server/tests"
  :author "Momozor"
  :license "AGPL-3.0"
  :depends-on ("clidle-server"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for clidle-server"
  :perform (test-op (op c) (symbol-call :rove :run c)))
