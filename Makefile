PROJECT_NAME=clidle
LISP ?= sbcl

all:
	$(LISP) --eval "(ql:quickload '($(PROJECT_NAME) $(PROJECT_NAME)-server))" \
		--eval "(asdf:make :$(PROJECT_NAME))" \
		--eval "(asdf:make :$(PROJECT_NAME)-server)" \
		--eval "(uiop:quit)"

