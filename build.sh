#ocamlopt -I +compiler-libs ocamlcommon.cmxa main.ml
#ocamlfind ocamlc -package compiler-libs.common main.ml

ocamlc -I +str str.cma -I +unix unix.cma -I +compiler-libs ocamlcommon.cma main.ml
