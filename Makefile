.PHONY: \
	build \
	rebuild \
	run \
	clean

rebuild:
	@$(MAKE) clean
	@$(MAKE) build
	@$(MAKE) run

build:
	@ocamlbuild -cflags '-w A' main.byte

run:
	@./main.byte

clean:
	@ocamlbuild -clean
