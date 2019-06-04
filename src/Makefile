
SUBDIRS = ast,util,rules,sat
LIBS = str
MAIN = main

OCAMLBUILD = ocamlbuild
OCAMLBUILDFLAGS = -use-ocamlfind -pkgs ocaml-protoc

DEBUG = $(MAIN).d.byte
NATIVE = $(MAIN).native
TOPLEVEL = $(MAIN).top

all: gver

gver: native
	install $(NATIVE) $@

proto: ../protobuf-mock/AST.proto
	cp ../protobuf-mock/AST.proto ast/ast.proto
	ocaml-protoc -binary -ml_out ast/ ast/ast.proto

debug: $(DEBUG)
native: $(NATIVE)
top: $(TOPLEVEL)

OCAMLOPTS = $(OCAMLBUILDFLAGS) -Is $(SUBDIRS) -libs $(LIBS)
OCAMLBRUN = $(OCAMLBUILD) $(OCAMLOPTS)

%.native: proto always
	@echo "[*] make workflow: .native"
	$(OCAMLBRUN) $@
%.d.byte: proto always
	@echo "[*] make workflow: d.byte"
	$(OCAMLBRUN) -cflags -annot $@
top: proto always
	@echo "[*] make workflow: top"
	$(OCAMLBRUN) $@

clean:
	$(OCAMLBUILD) -Is $(SUBDIRS) -clean
	@echo # $(OCAMLBUILD) -clean needs a newline
	rm -rf _build
	rm -rf main.native

TESTS = test/test.ml

test:
	ocamlbuild -use-ocamlfind -pkgs ocaml-protoc,oUnit -Is ast,util,rules,sat,test -libs str test/test_suite.ml test_suite.native
	install test_suite.native test_suite
	# ocamlfind ocamlc -o test/test_suite -package oUnit -linkpkg $(TESTS)

always:
	@echo "[!] make sure to run 'export DYLD_LIBRARY_PATH=~/.opam/default/lib/z3/' to set path for the dynamic library `z3lib.dylib`"

.PHONY: gver debug native top clean always test
