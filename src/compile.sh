echo "[*] compiling ast.proto"
ocaml-protoc -pp -binary -ml_out ./ ast.proto

echo "[*] compiling .mli and .ml files"
ocamlfind ocamlopt \
  -c -thread -linkpkg \
  -package core \
  ast_types.mli  ast_types.ml  \
  functools.mli  functools.ml  \
  utility.mli    utility.ml    \
  wellformed.mli wellformed.ml

echo "[*] building to main.native"
ocamlbuild -use-ocamlfind -pkgs ocaml-protoc -libs str -no-hygiene main.native
echo "[!] make sure to run 'export DYLD_LIBRARY_PATH=~/.opam/default/lib/z3/' to set path for the dynamic library `z3lib.dylib`"
