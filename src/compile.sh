echo "[*] compiling ast.proto"
ocaml-protoc -binary -ml_out ./ ast.proto

echo "[*] compiling .mli and .ml files"
ocamlopt -c ast_types.mli
ocamlopt -c ast_types.ml
ocamlopt -c functools.mli
ocamlopt -c functools.ml
ocamlopt -c utility.mli
ocamlopt -c utility.ml

# ocamlbuild -use-ocamlfind -pkgs ocaml-protoc -Is ast,util,rules,sat -libs str main.native
# + ocamlfind ocamlc -c -annot -bin-annot -thread -package ocaml-protoc,core,ppx_jane,ppx_compare,ppx_sexp_conv,getopt,z3,ppx_let -package ocaml-protoc -I rules -I ast -I sat -I util -o rules/formula.cmo rules/formula.ml

# ocamlbuild -use-ocamlfind -pkgs ocaml-protoc -libs str

echo "[*] building to main.native"
ocamlbuild -use-ocamlfind -pkgs ocaml-protoc -libs str ast.native
# ocamlfind ocamlopt -c
#   -annot -bin-annot -thread \
#   -package ocaml-protoc,core,ppx_jane,ppx_compare,ppx_sexp_conv,getopt,z3,ppx_let
#   -o ast_pb.cmo
#   ast_pb.ml
