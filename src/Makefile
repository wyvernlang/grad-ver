##############################################################################################################################
# global constants
##############################################################################################################################

#
# reference
#
SRC_DIRS_FLAG       = -I ast/ -I utility/ -I formula/ -I wellformed/ -I aliasing/ -I framing/ -I tests
BUILD_DIRS_FLAG 		= -I _build/ast/ -I _build/utility/ -I _build/formula/ -I _build/wellformed/ -I _build/aliasing/ \
											-I _build/framing/ -I _build/tests/
PKGS_FLAG 					= -package core,ppx_sexp_conv,sexplib,ocaml-protoc,ppx_deriving_protobuf,oUnit
LIBS_FLAG          	= -libs str

#
# build
#
OCAMLBUILD       		= ocamlbuild
OCAMLBUILD_FLAGS 		= -use-ocamlfind -tag thread $(PKGS_FLAG) $(LIBS_FLAG) $(SRC_DIRS_FLAG)
OCAMLBUILDRUN    		= $(OCAMLBUILD) $(OCAMLBUILD_FLAGS)

#
# executables
#
MAIN   							= main
TEST 							  = test
DEBUG				 				= debug

##############################################################################################################################
# compiling protobuf
##############################################################################################################################

proto:
	# compile ast.proto to ocaml files
	ocaml-protoc -pp -binary -ml_out ast/ ast/ast.proto
	# compile ocaml files
	make compile name=ast/ast_types
	make compile name=ast/ast_pp

##############################################################################################################################
# building ocaml
##############################################################################################################################

# build ocaml project and intstall main executable
build:
	# build ocaml project
	$(OCAMLBUILDRUN) $(MAIN).native
	# install main executable
	install $(MAIN).native $(MAIN)
	@echo "[!] make sure to run 'export DYLD_LIBRARY_PATH=~/.opam/default/lib/z3/' to set path for the dynamic library 'z3lib.dylib'"

clean:
	# clean ocamlbuild residue
	$(OCAMLBUILD) -clean
	@echo # $(OCAMLBUILD) -clean needs a newline
	# remove build artifacts
	rm -rf _build
	# remove executables
	rm $(MAIN)
	rm $(TEST)
	rm $(DEBUG)

##############################################################################################################################
# unit test
##############################################################################################################################

test: build
	# build ocaml project
	$(OCAMLBUILDRUN) $(TEST).native
	# install main executable
	install $(TEST).native $(TEST)
	# @echo "[!] make sure to run 'export DYLD_LIBRARY_PATH=~/.opam/default/lib/z3/' to set path for the dynamic library 'z3lib.dylib'"
	# run executable
	./$(TEST)

##############################################################################################################################
# debug - temporary executable for debugging stuff
##############################################################################################################################

debug: build
	# build ocaml project
	$(OCAMLBUILDRUN) $(DEBUG).native
	# install main executable
	install $(DEBUG).native $(DEBUG)
	# run executable
	./$(DEBUG)

##############################################################################################################################
# utilities
##############################################################################################################################

# infer ocaml interface
# outputs: _build/../${name}.inferred.mli
infer:
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) ${name}.inferred.mli


##############################################################################################################################
# documenting ocaml
##############################################################################################################################

DOCS_DIR = ../docs

docs:
	ocamlfind ocamldoc \
		$(BUILD_DIRS_FLAG) $(PKGS_FLAG) \
		-d $(DOCS_DIR) -html  \
		aliasing/*.mli ast/*.mli framing/*.mli utility/*.mli wellformed/*.mli # formula/*.mli

##############################################################################################################################
# Makefile metadata
##############################################################################################################################

.PHONY: proto build run clean all infer test debug docs
