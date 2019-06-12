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

INCLUDE_DIRS_FLAG = -I ast -I formula -I framing -I utility -I wellformed

OCAMLCOMPILE = ocamlfind ocamlopt -c -thread -linkpkg -package core,ocaml-protoc $(INCLUDE_DIRS_FLAG)

# ocaml interface inference

infer:
	# infer ocaml interface from implementation
	$(OCAMLCOMPILE) -i ${name}.ml > ${name}.mli

infer-all:
	# infer all ocaml interface from implementation
	$(OCAMLCOMPILE) -i utility/functools.ml > utility/functools.mli
	$(OCAMLCOMPILE) -i utility/utility.ml > utility/utility.mli
	$(OCAMLCOMPILE) -i ast/ast.ml > ast/ast.mli
	$(OCAMLCOMPILE) -i wellformed/wellformed.ml > wellformed/wellformed.mli
	$(OCAMLCOMPILE) -c framing/framing.mli framing/framing.ml

# ocaml compilation

compile:
	# compile ocaml interface and implementation
	$(OCAMLCOMPILE) -c ${name}.mli ${name}.ml

compile-all:
	# compile all ocaml interfaces and implementations, pairwise
	$(OCAMLCOMPILE) -c utility/functools.mli utility/functools.ml
	$(OCAMLCOMPILE) -c utility/utility.mli utility/utility.ml
	$(OCAMLCOMPILE) -c ast/ast.mli ast/ast.ml
	$(OCAMLCOMPILE) -c wellformed/wellformed.mli wellformed/wellformed.ml
	$(OCAMLCOMPILE) -c framing/framing.mli framing/framing.ml

compile-clean:
	# remove compiled ocaml
	rm \
		utility/${name}.cmx ast/${name}.cmx wellformed/${name}.cmx framing/${name}.cmx \
		utility/${name}.cmi ast/${name}.cmi wellformed/${name}.cmi framing/${name}.cmi

compile-clean-all:
	# remove all compiled ocaml
	rm utility/functools.cmx utility/functools.cmi
	rm utility/utility.cmx utility/utility.cmi
	rm ast/ast.cmx ast/ast.cmi
	rm wellformed/wellformed.cmx wellformed/wellformed.cmi
	rm framing/framing.cmx framing/framing.cmi

##############################################################################################################################
# building ocaml

MAIN   = main
NATIVE = $(MAIN).native

OCAMLBUILD = ocamlbuild
OCAMLBUILDFLAGS = -use-ocamlfind -no-hygiene -tag thread \
	-pkgs ocaml-protoc,core,ppx_deriving_protobuf \
	-libs str \
	$(INCLUDE_DIRS_FLAG)
OCAMLBUILDRUN = $(OCAMLBUILD) $(OCAMLBUILDFLAGS) $(NATIVE)

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

all: proto compile-all build

##############################################################################################################################
# documenting ocaml

DOCS_DIR = ../docs

docs:
	ocamlfind ocamldoc -html \
		-d $(DOCS_DIR)  \
		-package core,ocaml-protoc $(INCLUDE_DIRS_FLAG) \
		utility/*.mli ast/*.mli formula/*.mli framing/*.mli wellformed/*.mli

##############################################################################################################################
# Makefile metadata

.PHONY: proto compile build clean docs tmp
