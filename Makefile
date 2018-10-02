EXECUTABLE_NAME := test
EXECUTABLE_TYPE := native
EXECUTABLE_FILE := $(EXECUTABLE_NAME).$(EXECUTABLE_TYPE)

.PHONY: \
	all \
	build \
	rebuild \
	test \
	clean

all:
	@$(MAKE) rebuild
	@$(MAKE) test

rebuild:
	@$(MAKE) clean
	@$(MAKE) build

build:
	@ocamlbuild -cflags '-w A' -pkg str $(EXECUTABLE_FILE)

test:
	@cat ./test.data | ./$(EXECUTABLE_FILE)

clean:
	@ocamlbuild -clean
