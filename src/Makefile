
SUBDIRS = ast,util,rules,sat
LIBS = str
MAIN = main

OCAMLBUILD = ocamlbuild
OCAMLBUILDFLAGS = -use-ocamlfind -package ocaml-protoc

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
	$(OCAMLBRUN) $@
%.d.byte: proto always
	$(OCAMLBRUN) -cflags -annot $@
top: proto always
	$(OCAMLBRUN) $@

clean:
	$(OCAMLBUILD) -Is $(SUBDIRS) -clean
	@echo			# $(OCAMLBUILD) -clean needs a newline
	rm -rf _build
	rm -rf main.native

always:

.PHONY: gver debug native top clean always
