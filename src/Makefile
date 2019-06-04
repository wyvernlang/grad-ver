proto:
	# compile ast.proto to ocaml files
	ocaml-protoc -pp -binary -ml_out ./ ast.proto

compile:
	# compile functools lib to .cmxa
	ocamlopt -a -o functools.cmxa functools.mli functools.ml
	# compile ocaml files
	ocamlfind ocamlopt \
	  -c -thread -linkpkg \
	  -package core \
	  ast_types.mli  ast_types.ml  \
	  utility.mli    utility.ml    \
	  wellformed.mli wellformed.ml

MAIN   = main
NATIVE = $(MAIN).native

OCAMLBUILD      = ocamlbuild
OCAMLBUILDFLAGS = -use-ocamlfind -pkgs ocaml-protoc -libs str -no-hygiene
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
	rm -rf _build
	rm -rf main.native

all: proto compile build

.PHONY: proto compile build clean
