echo "[*] compiling ast.proto"
ocaml-protoc -pp -binary -ml_out ./ ast.proto

# ocamlfind ocamlopt -c -thread -linkpkg -package core #

echo "[*] compiling functools lib"
ocamlopt -a -o functools.cmxa functools.mli functools.ml

echo "[*] compiling .mli and .ml files"
ocamlfind ocamlopt \
  -c -thread -linkpkg \
  -package core \
  ast_types.mli  ast_types.ml  \
  utility.mli    utility.ml    \
  wellformed.mli wellformed.ml

echo "[*] building to main.native"
ocamlbuild -use-ocamlfind -pkgs ocaml-protoc -libs str -no-hygiene main.native
install main.native main
echo "[!] make sure to run 'export DYLD_LIBRARY_PATH=~/.opam/default/lib/z3/' to set path for the dynamic library 'z3lib.dylib'"
