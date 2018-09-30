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
	@ocamlbuild -cflags '-w A' pomdp.byte

run:
	@./pomdp.byte

clean:
	@ocamlbuild -clean
