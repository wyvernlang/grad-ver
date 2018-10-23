
MAIN = ast

OCAMLBUILD = ocamlbuild
OCAMLBUILDFLAGS = -use-ocamlfind -package ocaml-protoc

DEBUG = $(MAIN).d.byte
NATIVE = $(MAIN).native
TOPLEVEL = $(MAIN).top

all: ast

debug: $(DEBUG)
native: $(NATIVE)
top: $(TOPLEVEL)

OCAMLOPTS = $(OCAMLBUILDFLAGS) -Is $(SUBDIRS) -libs $(LIBS)
OCAMLBRUN = $(OCAMLBUILD) $(OCAMLOPTS)

ast: native

%.native: always
	$(OCAMLBRUN) $@
%.d.byte: always
	$(OCAMLBRUN) -cflags -annot $@
top: always
	$(OCAMLBRUN) $@

clean:
	$(OCAMLBUILD) -Is $(SUBDIRS) -clean
	@echo			# $(OCAMLBUILD) -clean needs a newline
	rm -f ../bin/c0c

always:

.PHONY: c0c debug native top clean always
