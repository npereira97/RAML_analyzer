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
			(*let s = Pprintast.string_of_structure parse_tree in*)
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
					| true -> Some x
					| false -> None)) xs) in 
		let funs = List.filter (fun x -> x != None) funs in 
		Some (List.map (function (Some x) -> x) funs) 

	let read_whole_file = (fun filename -> 
	    let ch = open_in filename in
	    let s = really_input_string ch (in_channel_length ch) in
	    close_in ch;
	    s)

	let code : string =  read_whole_file "main.ml"

	let all_funs = to_parsetree code >>= fun xs -> 
			Some (List.concat @@ List.map (fun x -> 
					  match get_binding_name x with 
					  | None -> []
					  | Some x -> [x]) xs)
		

	let get_files dir =  input_line @@ Unix.open_process_in @@  "find " ^ dir ^ " | grep -E \"*[.]ml$\" | tr '\n' ' '"

	let file_list dir = Str.split (Str.regexp " ") (get_files dir)

	let extract_funcs_from_file (func_names : string list) (file_name :string) = select_functions_ast func_names (read_whole_file file_name)


end



module Function_bank = struct 
	type t =  (string,  (string , Parsetree.structure) Hashtbl.t) Hashtbl.t

	let (>>=) = Extract.(>>=) 

	let load_module (file : string) = 
		let table = Hashtbl.create ~random:true 100 in 
		let _ = (Extract.to_parsetree @@ Extract.read_whole_file file) >>= (fun ts -> 
				 Some (List.map (fun t -> Extract.get_binding_name t >>= (fun name -> Some (Hashtbl.add table name t))) ts)) in 
		table

	let load_modules ?(dir=".") = 
		let files = Extract.file_list dir in 
		let _ = List.map print_endline files in
		let dir_len = String.length dir in 
		let module_names = List.map (fun s ->  ( String.make 1 @@ Char.uppercase_ascii @@ String.get s 0) ^ 
			 (String.sub s 1 ((String.length s) - 4)))
		 @@ 
			 List.map (fun s -> String.sub s (1 + dir_len) @@ (String.length s) - 1  - dir_len  ) files in
		let _ = List.map print_endline module_names in 
		let modules = List.map load_module files in 
		let table = Hashtbl.create ~random:true (List.length files) in
		let _ = List.map2 (fun name hashtable -> Hashtbl.add table name hashtable) module_names modules in 
		table 


	let _ = load_modules ~dir:"./testdir"

end 




