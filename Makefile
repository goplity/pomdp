EXECUTABLES     := test
EXECUTABLE_TYPE := native

.PHONY: \
	build \
	rebuild \
	test \
	clean

rebuild:
	@$(MAKE) -s clean
	@$(MAKE) -s build

build: $(foreach exe,$(EXECUTABLES),bin/$(exe))

bin/%: | bin
	@ocamlbuild -cflags '-w A -g' -pkg str $*.$(EXECUTABLE_TYPE)
	@rm $*.$(EXECUTABLE_TYPE)
	@cp _build/$*.$(EXECUTABLE_TYPE) bin/$*

bin:
	@mkdir -p bin

test:
	@./bin/test -gen 5 5 ran -e 0.01 -c 0.99

clean:
	@ocamlbuild -clean
	@rm -rf bin
