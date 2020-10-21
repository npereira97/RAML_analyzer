open Sexplib

type 'a sexp_helper = 
			{sexp_of_a : ('a -> Sexplib.Type.t) 
			;a_of_sexp : ( Sexplib.Type.t -> 'a)}



let list_helper helper = {sexp_of_a =  Sexplib.Std.sexp_of_list helper.sexp_of_a 
			; a_of_sexp = Sexplib.Std.list_of_sexp helper.a_of_sexp}

let string_helper = {
		sexp_of_a = Sexplib.Std.sexp_of_string;
		a_of_sexp = Sexplib.Std.string_of_sexp}


let write_to_disk (hw_path: string) (questions : string list)  = 
	let qs = List.map (fun (_,x)-> x) (Process.extract_funs hw_path questions) in 
	let sexp = ((list_helper string_helper).sexp_of_a) qs in 
	print_string (Sexp.to_string sexp)



let _ = write_to_disk  "~/Documents/fall-2020-students-code/hw3" ["map";"add_ingredient";"get_all_ingredients"]


