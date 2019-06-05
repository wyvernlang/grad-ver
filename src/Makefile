proto:
	# compile ast.proto to ocaml files
	ocaml-protoc -pp -binary -ml_out ./ ast.proto

OCAMLCOMPILE = ocamlfind ocamlopt -c -thread -package core

compile:
	# clean old compiles
	rm -f *.mli *.cmi *.o *.cmx

	# infer ocaml interfaces; compile ocaml interfaces and implementations
	# functools
	$(OCAMLCOMPILE) -i functools.ml > functools.mli
	$(OCAMLCOMPILE) functools.mli functools.ml
	# ast_types
	$(OCAMLCOMPILE) -i ast_types.ml > ast_types.mli
	$(OCAMLCOMPILE) ast_types.mli ast_types.ml
	# utility
	$(OCAMLCOMPILE) -i utility.ml > utility.mli
	$(OCAMLCOMPILE) utility.mli utility.ml
	# ast
	$(OCAMLCOMPILE) -i ast.ml > ast.mli
	$(OCAMLCOMPILE) ast.mli ast.ml
	# wellformed
	$(OCAMLCOMPILE) -i wellformed.ml > wellformed.mli
	$(OCAMLCOMPILE) wellformed.mli wellformed.ml

MAIN   = main
NATIVE = $(MAIN).native

OCAMLBUILD      = ocamlbuild
OCAMLBUILDFLAGS = -use-ocamlfind -pkgs ocaml-protoc -libs str,pbrt -no-hygiene
OCAMLBUILDRUN   = $(OCAMLBUILD) $(OCAMLBUILDFLAGS) $(NATIVE)

build:
	# build ocaml project
	$(OCAMLBUILDRUN)
	# install main
	install $(NATIVE) $(MAIN)
	@echo "[!] make sure to run 'export DYLD_LIBRARY_PATH=~/.opam/default/lib/z3/' to set path for the dynamic library 'z3lib.dylib'"

clean:
	# clean ocamlbuild residue
	$(OCAMLBUILD) -clean
	@echo # $(OCAMLBUILD) -clean needs a newline
	rm -rf _build main.native main

all: proto compile build

.PHONY: proto compile build clean
