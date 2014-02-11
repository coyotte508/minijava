open Value
open Expr
open Exceptions

let convert_to t v ctx = 
	{
		_type = t; 
		value = (match t with
		| Typing.TInt when v.value == NullValue -> IntValue (0)
		| Typing.TString when v.value == NullValue -> StringValue ("")
		| Typing.TBool when v.value == NullValue -> BoolValue (false)
		| Typing.TVoid -> NoValue
		| _ -> v.value)
	}

let store_var vname v ctx = 
	let (storage:scopedata) = ctx#get_var_data vname in 
	storage.value <- (convert_to storage._type v ctx).value; storage

let rec find_member_var value var = match value.value with
	| ClassValue (cval) -> (
		try 
			Hashtbl.find cval.attributes var
		with Not_found -> (
			try 
				let Parent p = cval.parent in find_member_var p var
			with
			| ExecutionError _ ->
				raise (ExecutionError ("Unexplained error, can't find " ^ var ^ " in class " ^ (Typing.type_to_string cval.actual_type)))
			| Match_failure _ ->
				raise (ExecutionError "Unexplained error")
		)
	)
	| NullValue -> raise (ExecutionError "Accessing attribute of null variable")

let rec get_id_storage id ctx = match id with
	| NamedIdent(x) -> ctx#get_var_data x
	| MemberVar(expr, x) -> 
		let evald = execute expr ctx in 
		find_member_var evald x

and get_function_data f ctx = match f with
	| NamedIdent(x) -> (void_value, ctx#get_wrapped_function x)
	| MemberVar(expr, x) -> 
		let evald = execute expr ctx in 
		(evald, find_member_var evald x)

and store_id id v ctx = 
	let storage = get_id_storage id ctx in
	storage.value <- (convert_to storage._type v ctx).value; storage

and execute node ctx = try (match node.expr with
	| Null -> {_type = Typing.TNull; value = NullValue }
	| Bool (b) -> make_bool b
	| Int (i) -> make_int i
	| String (s) -> {_type = Typing.TString;  value = StringValue (s) }
	| Unit -> void_value
	| VarCreation(v, expr) -> let ex = (execute expr ctx) in ctx#add_var v; store_var v.name ex ctx
	| Assign(id, expr) -> store_id id (execute expr ctx) ctx
	| Ident(id) -> get_id_storage id ctx
	| This -> ctx#this_val
	| New t -> ctx#generate_class_val t
	| Operation (left, op, right) ->
		let lval = execute left ctx and rval = execute right ctx in (match op with
		| Minus | Divided | Times | Mod | Lesser | Greater | LesserEq | GreaterEq -> 
			let IntValue(li) = lval.value and IntValue(ri) = rval.value in (match op with
			| Plus ->  make_int (li + ri)
			| Minus -> make_int (li - ri)
			| Divided -> make_int (li / ri)
			| Times -> make_int (li * ri)
			| Mod -> make_int (li mod ri)
			| Lesser -> make_bool (li < ri)
			| Greater -> make_bool (li > ri)
			| LesserEq -> make_bool (li <= ri)
			| GreaterEq -> make_bool (li >= ri)
			)
		| Plus -> (
			match (lval.value, rval.value) with
			| (IntValue li, IntValue ri) -> make_int (li + ri)
			| (StringValue s1, StringValue s2) -> make_string (s1 ^ s2)
		)
		| Eq -> make_bool (same_value lval.value rval.value)
		| NotEq -> make_bool (not (same_value lval.value rval.value))
		| And -> make_bool ((same_value lval.value rval.value) && (same_value lval.value (BoolValue true)))
		| Or -> make_bool ((same_value lval.value (BoolValue true)) or (same_value rval.value (BoolValue true))))
	| InstanceOf (expr, str) -> 
		make_bool (let ex = execute expr ctx in match ex.value with 
		| ClassValue cval -> ctx#inherits (Typing.type_to_string cval.actual_type) str
		| NullValue -> true
		| _ -> ctx#inherits (Typing.type_to_string ex._type) str)
	| Cast (str, expr) -> 
		let ex = execute expr ctx in (match ex.value with 
		| ClassValue cval -> 
			if not (ctx#inherits (Typing.type_to_string cval.actual_type) str) then 
				make_null (Typing.string_to_type str)
			else 
				convert_to (Typing.string_to_type str) ex ctx
		| _ ->
			convert_to (Typing.string_to_type str) ex ctx
	)
	| SOperation (op, expr) -> 
		let ex = execute expr ctx in (match op with 
		| SPlus -> ex
		| SMinus -> let IntValue i = ex.value in make_int (- i)
		| SNot -> let BoolValue b = ex.value in make_bool (not b))
	| ExpressionBlock (l) -> 
		execute_expr_block l ctx
	| FunctionCall (f, args) -> 
		let argvalues = List.map (fun x -> execute x ctx) args in
		execute_function f argvalues ctx
	| If (test, l) -> 
		let BoolValue b = (execute test ctx).value in 
		if b then ( 
			execute_expr_block l ctx; ()
		); void_value
	| IfElse (test, l1, l2) ->
		let BoolValue b = (execute test ctx).value in 
		if b then (
			execute_expr_block l1 ctx
		) else (
			execute_expr_block l2 ctx
		)
	| BuiltIn "print_int" -> let IntValue i = (ctx#get_var_data "_").value in print_int i; print_endline ""; void_value
	| BuiltIn "print_string" -> let StringValue i = (ctx#get_var_data "_").value in print_endline i; void_value
	) with e -> 
		print_endline ("[Running] Corresponding code: " ^ (dexpr_to_string node));
		raise e

and execute_expr_list l ctx = match l with 
    | [] -> void_value
    | hd::[] -> execute hd ctx
	| hd::tl -> execute hd ctx; execute_expr_list tl ctx

and execute_expr_block l ctx = 
	ctx#dive_into_scope;
	let res = execute_expr_list l ctx in
	ctx#exit_scope;
	res

and execute_function f args ctx =
	let (this, {_type = _; value = FunctionValue fobject} ) = get_function_data f ctx in 
	(
		ctx#stack_this this;
		ctx#dive_into_scope;
		let super_args = List.combine fobject.arguments args in 
		List.map (fun (att, value) -> ctx#add_var att; store_var att.name value ctx) super_args;
		let res = execute_expr_list fobject.body ctx in
		ctx#exit_scope;
		ctx#pop_this;
		convert_to fobject.return_type res ctx
	)

let rec execute_toplevel l ctx = match l with
	| Class (x) :: tl -> execute_toplevel tl ctx
	| Function (x) :: tl -> execute_toplevel tl ctx
	| Expression (x) :: tl -> execute x ctx; execute_toplevel tl ctx
	| [] -> ctx#clear_scope