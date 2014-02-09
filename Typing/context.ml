open Expr
open Exceptions

(* Can be extended to add info about location, ... *)
type scopedata = { _type: Typing.node_type }
type attributedef = { _type: Typing.node_type; init_val: expression }
type functiondef = { return_type: Typing.node_type; arg_types: Typing.node_type list }
type classdata = { attributedefs: (string, attributedef) Hashtbl.t;
					methoddefs:   (string, functiondef) Hashtbl.t
					}

class context =
	object (self)
        (* 
        	The last scope is the global scope. New scopes are put at the beginning of the list.
        	Contains string -> scopedata association
        *)
		val mutable scopes = [Hashtbl.create 0]
		val classes = Hashtbl.create 0
		val functions = Hashtbl.create 0
		method local_scope = List.hd scopes
		method add_var v = 
			if Hashtbl.mem self#local_scope v.name then
				raise GrammarError "Variable " ^ v.name ^ " already declared in local scope"
			else
				Hashtbl.add self#local_scope v.name {_type: (Typing.string_to_type v._type)}
		method type_of_var id =
			try 
				let data = self#get_var_data id in 
				data._type
			with GrammarError gerror ->
				if Hashtbl.mem functions id then Typing.TFunc((self#get_function_data f).return_type)
				else raise GrammarError gerror
		method type_of_function f =
			let ft = Typing.get_type f self in 
			match ft with
			 | Typing.TFunc(t) -> t 
			 | _ -> 
			 	raise GrammarError (ident_to_string f) ^ " is not a function, and as such can't be called."
		method type_of_membervar t id =
			match t with 
			| Typing.TClass(s) ->
				let class_data = self#get_class_data s in
				try
					let attr_data = Hashtbl.find class_data.attributedefs id in
					attr_data._type
				with Not_found ->
					(
						try 
							let func_data = Hashtbl.find class_data.methoddefs id in
							Typing.TFunc(func_data.return_type)
						with Not_found ->
							raise GrammarError "Attribute " ^ id ^ " unknown in class " ^ s
					)
			| _ ->
				raise GrammarError "Primitive type doesn't have attributes"
		method get_class_data c =
			try 
				Hashtbl.find classes c
			with Not_found ->
				raise GrammarError "Class " ^ c ^ " unknown."
		method get_function_data f = 
			try 
				Hashtbl.find functions f
			with Not_found ->
				raise GrammarError "Function " ^ f ^ " unknown."
		method get_var_data id =
			let rec get_scope_var_data id = function 
			| [] -> raise GrammarError "Variable " ^ id ^ " not declared"
			| hd::tl -> 
				if Hashtbl.mem hd id then Hashtbl.find hd id
				else get_scope_var_data id tl
			in get_scope_var_data id scopes
	end	