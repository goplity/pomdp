EXECUTABLE_NAME := test
EXECUTABLE_TYPE := byte
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
	@ocamlbuild -cflags '-w A' $(EXECUTABLE_FILE)

test:
	@./$(EXECUTABLE_FILE)

clean:
	@ocamlbuild -clean
