open Sexplib

type 'a sexp_helper = 
			{sexp_of_a : ('a -> Sexplib.Type.t) 
			;a_of_sexp : ( Sexplib.Type.t -> 'a)}



let list_helper helper = {sexp_of_a =  Sexplib.Std.sexp_of_list helper.sexp_of_a 
			; a_of_sexp = Sexplib.Std.list_of_sexp helper.a_of_sexp}

let string_helper = {
		sexp_of_a = Sexplib.Std.sexp_of_string;
		a_of_sexp = Sexplib.Std.string_of_sexp}


let string_of_sexp = string_helper.a_of_sexp
let list_of_sexp = Sexplib.Std.list_of_sexp

let sexp_of_string = string_helper.sexp_of_a
let sexp_of_list = Sexplib.Std.sexp_of_list

type student_solution = 
{name:string
;function_list : (string * string) list
;file_path:string} [@@deriving sexp]


let student_solution_helper = {sexp_of_a = sexp_of_student_solution;
		a_of_sexp = student_solution_of_sexp}


let emit_sexp (hw_path: string) (questions : string list)  = 
	let qs = List.map (fun (a,b,c)-> {name = a; function_list = b; file_path = c}   ) (Process.extract_funs hw_path questions) in 
	let sexp = (list_helper student_solution_helper).sexp_of_a qs in 
	print_string (Sexp.to_string sexp)



let _ = emit_sexp  "~/Documents/fall-2020-students-code/hw3" ["map";"add_ingredient";"get_all_ingredients"]


