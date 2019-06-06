##############################################################################################################################
# compiling protobuf

proto:
	# compile ast.proto to ocaml files
	ocaml-protoc -pp -binary -ml_out ./ ast.proto
	# compile ocaml files
	$(OCAMLCOMPILE) ast_types.mli ast_types.ml
	$(OCAMLCOMPILE) ast_pp.mli ast_pp.ml

##############################################################################################################################
# compiling ocaml

OCAMLCOMPILE = ocamlfind ocamlopt -c -thread -linkpkg -package core,ocaml-protoc

# ocaml interface inference

infer:
	# infer ocaml interface from implementation
	$(OCAMLCOMPILE) -i ${name}.ml > ${name}.mli

infer-all:
	# infer all ocaml interface from implementation
	$(OCAMLCOMPILE) -i functools.ml > functools.mli
	$(OCAMLCOMPILE) -i utility.ml > utility.mli
	$(OCAMLCOMPILE) -i ast.ml > ast.mli
	$(OCAMLCOMPILE) -i wellformed.ml > wellformed.mli
	$(OCAMLCOMPILE) -c framing.mli framing.ml

# ocaml compilation

compile:
	# compile ocaml interface and implementation
	$(OCAMLCOMPILE) -c ${name}.mli ${name}.ml

compile-all:
	# compile all ocaml interfaces and implementations, pairwise
	$(OCAMLCOMPILE) -c functools.mli functools.ml
	$(OCAMLCOMPILE) -c utility.mli utility.ml
	$(OCAMLCOMPILE) -c ast.mli ast.ml
	$(OCAMLCOMPILE) -c wellformed.mli wellformed.ml
	$(OCAMLCOMPILE) -c framing.mli framing.ml

compile-clean:
	# remove compiled ocaml
	rm ${name}.cmx ${name}.cmi

compile-clean-all:
	# remove all compiled ocaml
	rm functools.cmx functools.cmi
	rm utility.cmx utility.cmi
	rm ast.cmx ast.cmi
	rm wellformed.cmx wellformed.cmi
	rm framing.cmx framing.cmi

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

build-clean:
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
