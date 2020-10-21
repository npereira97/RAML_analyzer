#ocamlopt -I +compiler-libs ocamlcommon.cmxa main.ml
#ocamlfind ocamlc -package ocaml-compiler-libs.toplevel  main.ml



ocamlc -I +str str.cma -I +unix unix.cma -I +compiler-libs ocamlcommon.cma -a process.ml -o process.cma
ocamlfind ocamlc -I process process.cma -package sexplib nada.ml



#builds ppx
#ocamlc -o ppx_test.exe  -I +compiler-libs ocamlcommon.cma ppx_test.ml
