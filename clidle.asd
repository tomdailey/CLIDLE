(defsystem "clidle"
  :version "0.1.0"
  :author "Momozor"
  :license "AGPL-3.0"
  :depends-on ("ltk"
               "swank"
               "swank-client"
               "bt-semaphore"
               "cl-project")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "A Common Lisp beginner friendly IDLE"
  :build-operation "program-op" ;; leave as is
  :build-pathname "clidle"
  :entry-point "clidle:main"
  :in-order-to ((test-op (test-op "clidle/tests"))))

(defsystem "clidle/tests"
  :author "Momozor"
  :license "AGPL-3.0"
  :depends-on ("clidle"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for clidle"
  :perform (test-op (op c) (symbol-call :rove :run c)))
