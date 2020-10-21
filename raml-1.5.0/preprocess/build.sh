#ocamlopt -I +compiler-libs ocamlcommon.cmxa main.ml
#ocamlfind ocamlc -package ocaml-compiler-libs.toplevel  main.ml



ocamlfind ocamlc -I +str str.cma -I +compiler-libs ocamlcommon.cma -package sexplib,unix -a process.ml -o process.cma
ocamlfind ocamlc -I process process.cma -g -linkpkg -package ppx_sexp_conv,sexplib nada.ml



#builds ppx
#ocamlc -o ppx_test.exe  -I +compiler-libs ocamlcommon.cma ppx_test.ml
