EXECUTABLE_NAME := test
EXECUTABLE_TYPE := native
EXECUTABLE_TARGET := $(EXECUTABLE_NAME).$(EXECUTABLE_TYPE)

.PHONY: \
	all \
	build \
	rebuild \
	test \
	clean

all:
	@$(MAKE) -s rebuild
	@$(MAKE) -s test

rebuild:
	@$(MAKE) -s clean
	@$(MAKE) -s build

build: | bin
	@ocamlbuild -cflags '-w A' -pkg str $(EXECUTABLE_TARGET)
	@rm $(EXECUTABLE_TARGET)
	@cp _build/$(EXECUTABLE_TARGET) bin/$(EXECUTABLE_NAME)

bin:
	@mkdir -p bin

test:
	@./bin/$(EXECUTABLE_NAME)

clean:
	@ocamlbuild -clean
	@rm -rf bin
