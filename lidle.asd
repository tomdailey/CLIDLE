(defsystem "lidle"
  :version "0.1.0"
  :author "Momozor"
  :license "AGPL-3.0"
  :depends-on ("ltk"
               "swank-client")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "A Common Lisp beginner friendly IDLE"
  :build-operation "program-op" ;; leave as is
  :build-pathname "lidle"
  :entry-point "lidle:main"
  :in-order-to ((test-op (test-op "lidle/tests"))))

(defsystem "lidle/tests"
  :author "Momozor"
  :license "AGPL-3.0"
  :depends-on ("lidle"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for lidle"
  :perform (test-op (op c) (symbol-call :rove :run c)))
