target=main.byte
src=main.ml

.PHONY: run
run: $(target)
	/usr/bin/time ./$(target)

$(target): $(src)
	ocamlbuild -use-ocamlfind $@
