open Expr
open Exceptions

(* Can be extended to add info about location, ... *)
type scopedata = { _type: Typing.node_type }
type attributedef = { _type: Typing.node_type; init_val: expression }
type functiondef = { return_type: Typing.node_type; arguments: _attribute list; body: expression list }
type classdata = { attributedefs: (string, attributedef) Hashtbl.t;
					methoddefs:   (string, functiondef) Hashtbl.t;
					parent: string
					}

let func_type (func_data:functiondef) =
	let mtype =  func_data.return_type in 
	let arg_types = List.map (fun (x: _attribute) -> Typing.string_to_type x._type) func_data.arguments in
	Typing.TFunc(mtype, arg_types)

class context =
	let object_class = {attributedefs = Hashtbl.create 0; methoddefs = Hashtbl.create 0; parent=""} in
	object (self)
        (* 
        	The last scope is the global scope. New scopes are put at the beginning of the list.
        	Contains string -> scopedata association
        *)
		val mutable scopes = [Hashtbl.create 0]
		val mutable this_class = {_type = Typing.TVoid}
		val classes = (let c = Hashtbl.create 0 in Hashtbl.add c "Object" object_class; c)
		val functions = Hashtbl.create 0
		method local_scope = List.hd scopes
		method add_var (v: _attribute) = 
			if Hashtbl.mem self#local_scope v.name then
				raise (GrammarError ("Variable " ^ v.name ^ " already declared in local scope"))
			else
				Hashtbl.add self#local_scope v.name {_type = (Typing.string_to_type v._type)}
		method type_of_var id =
			try 
				let data = self#get_var_data id in 
				data._type
			with GrammarError gerror ->
				if Hashtbl.mem functions id then func_type (self#get_function_data id)
				else raise (GrammarError gerror)
		method dive_into_scope = 
			scopes <- (Hashtbl.create 0) :: scopes
		method exit_scope =
			assert ((List.length scopes) > 1);
			scopes <- List.tl scopes
		method dive_into_class c =
			self#dive_into_scope;
			assert (this_class._type == Typing.TVoid);
			this_class <- {_type=TClass(c)}
		method exit_class =
			this_class <- {_type = Typing.TVoid};
			self#exit_scope
		method dive_into_function (f: _method) =
			self#dive_into_scope;
			List.map self#add_var f.arguments;
		method exit_function =
			self#exit_scope
		method type_of_function f =
			let ft = Typing.get_type (dref (Ident f)) self in 
			match ft with
			 | Typing.TFunc(t, _) -> t 
			 | _ -> 
			 	raise (GrammarError ((ident_to_string f) ^ " is not a function, and as such can't be called."))
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
							func_type func_data
						with Not_found ->
							try 
								if class_data.parent != "Object" then 
									self#type_of_membervar (Typing.TClass(class_data.parent)) id
								else
									raise (GrammarError "dummy")
							with GrammarError _ ->
								raise (GrammarError ("Attribute " ^ id ^ " unknown in class " ^ s))
					)
			| _ ->
				raise (GrammarError ("Primitive type of " ^ id ^ " doesn't have attributes"))
		method has_member_var t id =
			try
				self#type_of_membervar t id;
				true
			with GrammarError _ -> false
		method this_type =
			if this_class._type == TVoid then raise (GrammarError "this keyword used outside a class")
			else this_class._type
		method get_class_data c =
			try 
				Hashtbl.find classes c
			with Not_found ->
				raise (GrammarError ("Class " ^ c ^ " unknown."))
		method get_function_data f = 
			try 
				Hashtbl.find functions f
			with Not_found ->
				raise (GrammarError ("Function " ^ f ^ " unknown."))
		method get_var_data : string -> scopedata = fun id ->
			let rec get_scope_var_data id = function 
			| [] -> raise (GrammarError ("Variable " ^ id ^ " not declared"))
			| hd::tl -> 
				if Hashtbl.mem hd id then Hashtbl.find hd id
				else get_scope_var_data id tl
			in get_scope_var_data id scopes
		method add_class (c: _class) =
			if Hashtbl.mem classes c.name then
				raise (GrammarError ("Class " ^ c.name ^ " already declared, and redefined."));
			
			if (c.parent != "Object" && not (Hashtbl.mem classes c.parent)) then
				raise (GrammarError ("Class " ^ c.name ^ " inherits " ^ c.parent ^ ", so " ^ c.parent ^ " must be declared before " ^ c.name ^ "! (long live C++)"));

			let cdata = {
				parent = c.parent;
				attributedefs = Hashtbl.create 0;
				methoddefs = Hashtbl.create 0;
			} in (
				let rec add_f (flist: _method list) = (match flist with
					| f::tl ->
						let value = { return_type = Typing.string_to_type f.return_type; 
															  arguments = f.arguments;
															  body = f.body
															} in 
						( 
							if self#has_member_var (Typing.TClass c.parent) f.name then (
								let t1 = (self#type_of_membervar (Typing.TClass c.parent) f.name) in 
								let t2 = func_type value in
								(
									(* == doesn't work *)
									if String.compare (Typing.type_to_string t1) (Typing.type_to_string t2) != 0 then (
										raise (GrammarError ("Function " ^ f.name ^ " of class " ^ c.name ^ 
												" already declared in superclass, with an incompatible type/signature: " ^
											   (Typing.type_to_string t1) ^ " as opposed to " ^ (Typing.type_to_string t2)))
									);
								)
							);

							if Hashtbl.mem cdata.methoddefs f.name then
								raise (GrammarError ("Method " ^ f.name ^ " of class " ^ c.name ^ " already declared, and redefined."));

							Hashtbl.add cdata.methoddefs f.name value
						); 
						add_f tl
					| _ -> ()
				) in add_f c.methods;
				let rec add_a (alist: var list) = (match alist with
					| (a, expr)::tl -> 
						if self#has_member_var (Typing.TClass c.parent) a.name then
							raise (GrammarError ("Attribute " ^ a.name ^ " of class " ^ c.name ^ " already declared in superclass."));
						if Hashtbl.mem cdata.attributedefs a.name then
							raise (GrammarError ("Attribute " ^ a.name ^ " of class " ^ c.name ^ " already declared, and redefined."));

						Hashtbl.add cdata.attributedefs a.name { _type = Typing.string_to_type a._type; init_val = expr };
						add_a tl
					| _ -> ()
				) in add_a c.attributes;
				Hashtbl.add classes c.name cdata
			)
		method add_function f =
			if Hashtbl.mem functions f.name then
				raise (GrammarError ("Function " ^ f.name ^ " already declared, and redefined."));

				Hashtbl.add functions f.name { return_type = Typing.string_to_type f.return_type; 
											   arguments = f.arguments;
							 				   body = f.body
											 };
	end