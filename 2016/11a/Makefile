target=main.native
src=main.ml

.PHONY: run
run: $(target)
	./$(target)

$(target): $(src)
	ocamlbuild -use-ocamlfind $@
