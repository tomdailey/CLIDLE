(defsystem "clidle-server"
    :version "0.1.0"
    :author "Momozor"
    :license "GPL-3.0"
    :depends-on ("swank" 
                 "com.google.base")
    :components ((:module "src"
                          :components
                          ((:file "main"))))
    :build-operation "program-op" ;; leave as is
    :build-pathname "clidle-server"
    :entry-point "clidle-server:main"
    :description "Server side for CLIDLE")
