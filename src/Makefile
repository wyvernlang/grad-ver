##############################################################################################################################
# compiling ocaml

OCAMLCOMPILE = ocamlfind ocamlopt -thread -linkpkg -package core,ocaml-protoc

proto:
	# compile ast.proto to ocaml files
	ocaml-protoc -pp -binary -ml_out ./ ast.proto
	# compile ocaml files
	$(OCAMLCOMPILE) ast_types.mli ast_types.ml
	$(OCAMLCOMPILE) ast_pp.mli ast_pp.ml

compile:
	# infer ocaml interfaces; compile ocaml interfaces and implementations
	# functools
	rm functools.cmx functools.cmi
	$(OCAMLCOMPILE) -i functools.ml > functools.mli
	$(OCAMLCOMPILE) -c functools.mli functools.ml
	# utility
	rm utility.cmx utility.cmi
	$(OCAMLCOMPILE) -i utility.ml > utility.mli
	$(OCAMLCOMPILE) -c utility.mli utility.ml
	# ast
	rm ast.cmx ast.cmi
	$(OCAMLCOMPILE) -i ast.ml > ast.mli
	$(OCAMLCOMPILE) -c ast.mli ast.ml
	# wellformed
	rm wellformed.cmx wellformed.cmi
	$(OCAMLCOMPILE) -i wellformed.ml > wellformed.mli
	$(OCAMLCOMPILE) -c wellformed.mli wellformed.ml

##############################################################################################################################
# building ocaml

MAIN   = main
NATIVE = $(MAIN).native

OCAMLBUILD      = ocamlbuild
OCAMLBUILDFLAGS = -use-ocamlfind -no-hygiene -pkgs ocaml-protoc,core,ppx_deriving_protobuf -tag thread -libs str
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

##############################################################################################################################
# documenting ocaml

DOCS_DIR = ../docs

docs:
	ocamlfind ocamldoc -html -d $(DOCS_DIR) -package core,ocaml-protoc *.mli

##############################################################################################################################
# Makefile metadata

.PHONY: proto compile build clean docs tmp
