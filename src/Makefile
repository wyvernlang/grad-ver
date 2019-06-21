##############################################################################################################################
# global constants

# reference
SRC_DIRS_FLAG       = -I ast/ -I utility/ -I formula/ -I aliasing/ -I framing/ -I wellformed/
BUILD_DIRS_FLAG 		= -I _build/ast/ -I _build/utility/ -I _build/aliasing/ -I _build/framing/ -I _build/wellformed/ # -I _build/formula/
PKGS_FLAG 					= -package core,ppx_sexp_conv,sexplib,ocaml-protoc,ppx_deriving_protobuf
LIBS_FLAG          	= -libs str
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

infer:
	# infer ocaml interface
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) ${name}.inferred.mli

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
