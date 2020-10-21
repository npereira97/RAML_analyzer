

module Extract = struct 
	open Parsetree

	let exception_to_option f = (fun x -> try
						Some (f x)
						with
						| _ -> None)

	let (>>=) x f = match x with 
			| None -> None
			| Some x -> f x

	let get_binding_name p =  
		match p.pstr_desc with
		| Pstr_value (_,xs) -> begin 
					match xs with 
					| [] -> None
					| x :: _ -> match x.pvb_pat.ppat_desc with
							| Ppat_var x -> Some x.txt
							| _ -> None
					end
		| _ -> None

	let to_parsetree s = 
		try
			let buf = Lexing.from_string s in 
			let parse_tree = Parse.implementation buf in 
			Some parse_tree
		with
		| _ -> None

	let fun_list = ["code";"select_functions";"b"]

	let code = "let rec t = 1 \n let g = (fun x -> x)\n let f x = x"

	let select_functions (fun_list : string list) (code : string) : string list option= 
		to_parsetree code >>= fun xs -> 
		let funs =  (List.map (fun x -> get_binding_name x >>= (fun s -> 
					match List.exists (fun a -> a = s) fun_list with 
					| true -> Some (Pprintast.string_of_structure [x])
					| false -> None)) xs) in 
		let funs = List.filter (fun x -> x != None) funs in 
		Some (List.map (function (Some x) -> x) funs) 


	let select_functions_ast (fun_list : string list) (code : string)= 
		to_parsetree code >>= fun xs -> 
		let funs =  (List.map (fun x -> get_binding_name x >>= (fun s -> 
					match List.exists (fun a -> a = s) fun_list with 
					| true -> Some (s,Pprintast.string_of_structure [x])
					| false -> None)) xs) in 
		let funs = List.filter (fun x -> x != None) funs in 
		Some (List.map (function (Some x) -> x) funs) 

	let read_whole_file = (fun filename -> 
	    let ch = open_in filename in
	    let s = really_input_string ch (in_channel_length ch) in
	    close_in ch;
	    s)

	let parse_tree_of_file s = to_parsetree @@ read_whole_file s

	(*let code : string =  read_whole_file "main.ml"*)

	let all_funs = to_parsetree code >>= fun xs -> 
			Some (List.concat @@ List.map (fun x -> 
					  match get_binding_name x with 
					  | None -> []
					  | Some x -> [x]) xs)


	let path_to_name = let rec last xs = match xs with
						| [x] -> x 
						| x :: xs -> last xs in 
						(fun s -> last @@  Str.split (Str.regexp "/") s)
		

	let get_files dir =  input_line @@ Unix.open_process_in @@  "find " ^ dir ^ " | grep -E \"*[.]ml$\" | tr '\n' ' '"

	let file_list dir = Str.split (Str.regexp " ") (get_files dir)

	let extract_funcs_from_file (func_names : string list) (file_name :string)= (fun (Some x) -> x) @@ select_functions_ast func_names (read_whole_file file_name)


end

let (>>=) = Extract.(>>=)


let test_mapper argv =
	let open Asttypes in 
	let open Parsetree in 
	let open Ast_mapper in 
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc =  Pexp_ident { txt = Ldot (Lident modu ,func) ;loc = l } } ->
       	let _ = print_endline modu in 
        expr
      | other -> default_mapper.expr mapper other; }



module Function_bank = struct 
	type t =  (string,  (string , Parsetree.structure) Hashtbl.t) Hashtbl.t

	let (>>=) = Extract.(>>=) 

	let (>|=) x (f,alt) = 
		match x >>= f with
		| None -> alt 
		| Some a -> Some a


	let load_module (file : string) : (string , Parsetree.structure) Hashtbl.t = 
		let table = Hashtbl.create ~random:true 100 in 
		let _ = (Extract.to_parsetree @@ Extract.read_whole_file file) >>= (fun ts -> 
				 Some (List.map (fun t -> Extract.get_binding_name t >>= (fun name -> Some (Hashtbl.add table name [t]))) ts)) in 
		table

	let load_modules ?(dir=".") =
		let rec last xs = match xs with
						| [x] -> x 
						| x :: xs -> last xs in 
		let files = Extract.file_list dir in 
		let dir_len = String.length dir in 
		let module_names = List.map (fun s ->  ( String.make 1 @@ Char.uppercase_ascii @@ String.get s 0) ^ 
			 (String.sub s 1 ((String.length s) - 4)))
		 @@ 
			 List.map (fun s -> last @@  Str.split (Str.regexp "/") s) files in
		let modules = List.map load_module files in 
		let table = Hashtbl.create ~random:true (List.length files) in
		let _ = List.map2 (fun name hashtable -> Hashtbl.add table name hashtable) module_names modules in 
		table 


	let lookup (function_bank:t) module_name function_name : Parsetree.structure option = 
		Hashtbl.find_opt function_bank module_name >>= (fun _mod -> 
			Hashtbl.find_opt _mod function_name
		)



end 



let coerce = (fun (Some x) -> x)

let string_of_structure = Pprintast.string_of_structure


let extract_funs (hw_path:string) (fun_list:string list) =

  		let paths = Extract.file_list hw_path in 
		let file_names = List.map Extract.path_to_name paths in
		let funcs = List.map (Extract.extract_funcs_from_file fun_list) paths in 
		let f = (fun x -> match x with 
					| None -> []
					| Some x -> x) in
		let inter = List.map2 (fun name func_list ->  
					(name,func_list))  file_names funcs in

		List.map2 (fun (a,b) c -> (a,b,c)) inter paths




