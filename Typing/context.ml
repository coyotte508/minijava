open Expr
open Exceptions
open Value

let func_type (func_data:functiondef) =
	let mtype =  func_data.return_type in 
	let arg_types = List.map (fun (x: _attribute) -> Typing.string_to_type x._type) func_data.arguments in
	Typing.TFunc(mtype, arg_types)

class context =
	let object_class = {attributedefs = Hashtbl.create 0; methoddefs = Hashtbl.create 0; parent=""} in
	let print_int = { return_type = Typing.TVoid; arguments = [{_type="Int"; name="_"}]; body = [dref (BuiltIn "print_int")]} in 
	let print_string = { return_type = Typing.TVoid; arguments = [{_type="String"; name="_"}]; body = [dref (BuiltIn "print_string")]} in 
	object (self)
        (* 
        	The last scope is the global scope. New scopes are put at the beginning of the list.
        	Contains string -> scopedata association
        *)
		val mutable scopes = [Hashtbl.create 0]
		val mutable this_class = [void_value]
		val classes = (let c = Hashtbl.create 0 in Hashtbl.add c "Object" object_class; c)
		val functions = (let c = Hashtbl.create 0 in Hashtbl.add c "print_string" print_string; Hashtbl.add c "print_int" print_int; c) 
		method local_scope = List.hd scopes
		method clear_scope =
			assert (List.length scopes == 1); 
			scopes <- [Hashtbl.create 0]
		method add_var (v: _attribute) = 
			if Hashtbl.mem self#local_scope v.name then
				raise (GrammarError ("Variable " ^ v.name ^ " already declared in local scope"))
			else
				Hashtbl.add self#local_scope v.name {_type = (Typing.string_to_type v._type); value = NoValue}
		method type_of_var id =
			try 
				let data = self#get_var_data id in 
				data._type
			with GrammarError gerror ->
				if Hashtbl.mem functions id then func_type (self#get_function_data id)
				else if self#has_member_var self#this_type id then self#type_of_membervar self#this_type id
				else raise (GrammarError gerror)
		method dive_into_scope = 
			scopes <- (Hashtbl.create 0) :: scopes
		method exit_scope =
			assert ((List.length scopes) > 1);
			scopes <- List.tl scopes
		method dive_into_class c =
			self#dive_into_scope;
			assert ((List.length this_class) == 1);
			self#stack_this {_type=TClass(c); value = NoValue}
		method exit_class =
			self#pop_this;
			self#exit_scope
		method stack_this this = 
			this_class <- this::this_class
		method pop_this =
			assert ((List.length this_class) > 1);
			this_class <- (List.tl this_class) 
		method dive_into_function (f: _method) =
			self#dive_into_scope;
			List.map self#add_var f.arguments;
		method exit_function =
			self#exit_scope
		method returntype_of_function f =
			let ft = Typing.get_type (dref (Ident f)) self in 
			match ft with
			 | Typing.TFunc(t, _) -> t 
			 | _ -> 
			 	raise (GrammarError ((ident_to_string f) ^ " is not a function, and as such can't be called."))
		method type_of_function f =
			let ft = Typing.get_type (dref (Ident f)) self in 
			match ft with
			 | Typing.TFunc(t, _) -> ft
			 | _ -> 
			 	raise (GrammarError ((ident_to_string f) ^ " is not a function, and as such can't be called."))
		method type_implicitly_castable a b =
			match (a, b) with
			| _ when Typing.same_type a b -> true 
			| (Typing.TNull, _) -> true
			| (_, Typing.TObject) -> true
			| (Typing.TClass c1, Typing.TClass c2) -> self#inherits c1 c2
			| _ -> false
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
			if self#this_val._type == TVoid then raise (GrammarError "this keyword used outside a class")
			else self#this_val._type
		method this_val : scopedata = List.hd this_class
		method get_class_data c =
			try 
				Hashtbl.find classes c
			with Not_found ->
				raise (GrammarError ("Class " ^ c ^ " unknown."))
		method inherits a b =
			if (String.compare b "Object") == 0 or (String.compare b a) == 0 or (String.compare a "Null") == 0 then true 
			else (
				if (String.compare a "Object") == 0 then false else
				(
					try let parent = (self#get_class_data a).parent in self#inherits parent b
					with GrammarError gerror -> false
				)
			) 
		method get_wrapped_function f = 
			let x = self#get_function_data f in 
			{_type = func_type x; value = FunctionValue x}
		method get_function_data f = 
			try 
				(try 
					Hashtbl.find functions f
				with Not_found ->
					raise (GrammarError ("Function " ^ f ^ " unknown.")))
			with GrammarError gerror ->
				(* If the function isn't global, try to find it as a member variable of the current class *)
				try 
					let (x:scopedata) = self#find_member_var self#this_val f in
					(match x.value with
					| FunctionValue(fdef) -> fdef)
				with _ ->
					raise (GrammarError gerror)
		method ensure_return_types =
			Hashtbl.iter (fun x y -> self#ensure_class_return_types x y) classes;
			Hashtbl.iter (fun x y -> self#ensure_function_return_type x y) functions;
		method ensure_class_return_types cname cdata =
			try 
				Hashtbl.iter (fun x y -> self#ensure_function_return_type x y) cdata.methoddefs;
				Hashtbl.iter (fun x y -> self#ensure_attribute_return_type x y) cdata.attributedefs;
			with GrammarError gerror ->
				raise (GrammarError ("Inside class " ^ cname ^ ": " ^ gerror)) 
		method ensure_attribute_return_type aname adata =
			let rt = Typing.get_type adata.init_val self in
			if not (self#type_implicitly_castable rt adata._type) then
				raise (GrammarError ("Type of attribute " ^ aname ^ " is declared as " ^ (Typing.type_to_string adata._type)
									 ^ " but is initialized as " ^ (Typing.type_to_string rt)))  
		method ensure_function_return_type fname fdata =
			let rt = Typing.get_expr_list_type fdata.body self in
			if fdata.return_type != Typing.TVoid && (not (self#type_implicitly_castable rt fdata.return_type)) then
				raise (GrammarError ("Return type of function " ^ fname ^ " is declared as " ^ (Typing.type_to_string fdata.return_type)
									 ^ " but return value is " ^ (Typing.type_to_string rt)))  
		method generate_class_val cname = 
			let cdata = Hashtbl.find classes cname in 
			let pdata = match cdata.parent with
			| "" -> NoParent
			| parent -> Parent (self#generate_class_val parent) in
			let attr_to_attr aname attr tbl =
				Hashtbl.add tbl aname (Execute.convert_to attr._type (Execute.execute attr.init_val self) self); tbl in
			let attrs = Hashtbl.fold attr_to_attr cdata.attributedefs (Hashtbl.create 0) in
			let meth_to_attr mname meth tbl = 
				Hashtbl.add tbl mname {_type = func_type meth; value = FunctionValue meth}; tbl in
			{
				_type = Typing.string_to_type cname;
				value = ClassValue {
					actual_type = Typing.string_to_type cname;
					parent = pdata;
					attributes = Hashtbl.fold meth_to_attr cdata.methoddefs attrs
				}
			}
		method find_member_var value var = match value.value with
			| ClassValue (cval) -> (
				try 
					let (x:scopedata) = Hashtbl.find cval.attributes var in x
				with Not_found -> (
					try 
						let Parent p = cval.parent in self#find_member_var p var
					with
					| ExecutionError _ ->
						raise (ExecutionError ("Unexplained error, can't find " ^ var ^ " in class " ^ (Typing.type_to_string cval.actual_type)))
					| Match_failure _ ->
						raise (ExecutionError "Unexplained error")
				)
			)
			| NullValue -> raise (ExecutionError "Accessing attribute of null variable")
		method get_var_data : string -> scopedata = fun id ->
			try
				begin 
					let rec get_scope_var_data id = function 
					| [] -> raise (GrammarError ("Variable " ^ id ^ " not declared"))
					| hd::tl -> 
						if Hashtbl.mem hd id then Hashtbl.find hd id
						else get_scope_var_data id tl
					in get_scope_var_data id scopes
				end
			with GrammarError gerror ->
				begin 
					(* If the variable isn't in the scopes, try to find it as a member variable of the current class *)
					try 
						let x = self#find_member_var self#this_val id in
						begin
							assert (not (Typing.is_function_type(x._type)));
							x 
						end
					with error ->
						raise (GrammarError gerror)
				end
		method add_class (c: _class) =
			if Hashtbl.mem classes c.name or (String.compare c.name "Int" == 0) or (String.compare c.name "Bool" == 0) 
				or (String.compare c.name "String" == 0) or (String.compare c.name "Void" == 0) then
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
									if not (Typing.same_type t1 t2) then (
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