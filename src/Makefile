##############################################################################################################################
# global constants

# reference
SRC_DIRS_FLAG       = -I ast/ -I utility/ -I formula/ -I aliasing/ -I framing/ -I wellformed/
BUILD_DIRS_FLAG 		= -I _build/ast/ -I _build/utility/ -I _build/aliasing/ -I _build/framing/ -I _build/wellformed/ # -I _build/formula/
PKGS_FLAG 					= -package core,ppx_sexp_conv,sexplib,ocaml-protoc,ppx_deriving_protobuf
LIBS_FLAG          	= -libs str
# compile
OCAMLCOMPILE 			 	= ocamlfind ocamlopt
OCAMLCOMPILE_FLAGS 	= -thread -linkpkg
OCAMLCOMPILERUN    	= $(OCAMLCOMPILE) $(OCAMLCOMPILE_FLAGS) $(PKGS_FLAG) $(SRC_DIRS_FLAG)
# build
OCAMLBUILD       		= ocamlbuild
OCAMLBUILD_FLAGS 		= -use-ocamlfind -tag thread $(PKGS_FLAG) $(LIBS_FLAG) $(SRC_DIRS_FLAG)
OCAMLBUILDRUN    		= $(OCAMLBUILD) $(OCAMLBUILD_FLAGS) $(MAIN).native
# main
MAIN   							= main
NATIVE 							= $(MAIN).native

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
	$(OCAMLCOMPILERUN) -c -i ${name}.ml # > ${name}.mli

# ocaml compilation

compile:
	# compile ocaml interface and implementation
	$(OCAMLCOMPILERUN) -c ${name}.mli ${name}.ml

compile-all:
	# compile all ocaml interfaces and implementations, pairwise
	$(OCAMLCOMPILERUN) -c utility/functools.mli utility/functools.ml
	$(OCAMLCOMPILERUN) -c utility/utility.mli utility/utility.ml
	$(OCAMLCOMPILERUN) -c ast/ast.mli ast/ast.ml
	$(OCAMLCOMPILERUN) -c wellformed/wellformed.mli wellformed/wellformed.ml
	$(OCAMLCOMPILERUN) -c framing/framing.mli framing/framing.ml

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

clean:
	# clean ocamlbuild residue
	$(OCAMLBUILD) -clean
	@echo # $(OCAMLBUILD) -clean needs a newline
	rm -rf _build main.native main

all: proto compile-all build

##############################################################################################################################
# documenting ocaml

DOCS_DIR = ../docs

docs:
	ocamlfind ocamldoc \
		$(BUILD_DIRS_FLAG) $(PKGS_FLAG) \
		-d $(DOCS_DIR) -html  \
		aliasing/*.mli ast/*.mli framing/*.mli utility/*.mli wellformed/*.mli # formula/*.mli

##############################################################################################################################
# temporary

#

##############################################################################################################################
# Makefile metadata

.PHONY: proto compile build clean docs tmp aliasing
