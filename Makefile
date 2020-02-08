PROJECT_NAME=lidle
LISP ?= sbcl

all:
	$(LISP) --eval "(ql:quickload :$(PROJECT_NAME))" \
                --eval "(asdf:make :$(PROJECT_NAME))" \
		--eval "(uiop:quit)"

