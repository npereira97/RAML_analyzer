open Sexplib

type 'a sexp_helper = 
			{sexp_of_a : ('a -> Sexplib.Type.t) 
			;a_of_sexp : ( Sexplib.Type.t -> 'a)}



let list_helper helper = {sexp_of_a =  Sexplib.Std.sexp_of_list helper.sexp_of_a 
			; a_of_sexp = Sexplib.Std.list_of_sexp helper.a_of_sexp}

let string_helper = {
		sexp_of_a = Sexplib.Std.sexp_of_string;
		a_of_sexp = Sexplib.Std.string_of_sexp}



let _ = print_string "hola"
