##############################################################################################################################
# global constants

# reference
DIRS_FLAG = -I ast -I utility -I formula -I aliasing -I framing -I wellformed
PKGS_FLAG = -package core,ppx_sexp_conv,sexplib,ocaml-protoc,ppx_deriving_protobuf
LIBS_FLAG = -libs str
# compile
OCAMLCOMPILE = ocamlfind ocamlopt -c -thread -linkpkg $(PKGS_FLAG) $(DIRS_FLAG)
# build
OCAMLBUILD      = ocamlbuild
OCAMLBUILDFLAGS = -use-ocamlfind -no-hygiene -tag thread $(PKGS_FLAG) $(LIBS_FLAG) $(DIRS_FLAG)
OCAMLBUILDRUN   = $(OCAMLBUILD) $(OCAMLBUILDFLAGS) $(MAIN).native

# main
MAIN   = main
NATIVE = $(MAIN).native

##############################################################################################################################
# compiling protobuf

proto:
	# compile ast.proto to ocaml files
	ocaml-protoc -pp -binary -ml_out ast/ ast/ast.proto
	# compile ocaml files
	make compile name=ast/ast_types
	make compile name=ast/ast_pp

##############################################################################################################################
# compiling ocaml

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

build:
	# build ocaml project
	$(OCAMLBUILDRUN)
	# install main
	install $(MAIN).native $(MAIN)
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
	ocamlfind ocamldoc -html -d \
		$(DOCS_DIR) $(PKGS_FLAG) $(DIRS_FLAG) \
		utility/*.mli ast/*.mli formula/*.mli framing/*.mli wellformed/*.mli

##############################################################################################################################
# temporary

aliasing:


##############################################################################################################################
# Makefile metadata

.PHONY: proto compile build clean docs tmp aliasing
