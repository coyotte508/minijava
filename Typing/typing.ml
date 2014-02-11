open Expr 
open Exceptions

type node_type =
	| TInt
	| TBool
	| TString
	| TObject
	| TVoid
	| TNull
	| TClass of string
	| TFunc of node_type * node_type list

(* Can be extended to add info about location, ... *)
type metadata = { _type: node_type }

let metatable = Hashtbl.create 0;;

let last_of_list l = List.nth l ((List.length l)-1)

let has_type node =
	try 
		let x = Hashtbl.find metatable node in true
	with Not_found ->
		false

let string_to_type = function
	| "Bool" -> TBool
	| "Int" -> TInt
	| "Void" -> TVoid
	| "String" -> TString
	| "Object" -> TObject
	| "Null" -> TNull
	| s -> TClass (s)

let rec type_to_string = function 
	| TBool -> "Bool"
	| TInt -> "Int"
	| TObject -> "Object"
	| TString -> "String"
	| TVoid -> "Void"
	| TNull -> "Null"
	| TClass s -> s
	| TFunc(t1, l) -> (type_to_string t1) ^ "(" ^ (String.concat ", " (List.map type_to_string l)) ^ ")"

let same_type a b = (String.compare (type_to_string a) (type_to_string b)) == 0

let rec get_type node ctx =
	try
		let x = Hashtbl.find metatable node in 
		x._type
	with Not_found ->
		let t = (match node.expr with
		| Null -> TNull
		| Bool (b) -> TBool
		| Int (i) -> TInt
		| String (s) -> TString
		| Unit -> TVoid
		| VarCreation(v, _) -> string_to_type(v._type)
		| Assign(id, _) -> get_ident_type id ctx
		| Ident(id) -> get_ident_type id ctx
		| This -> ctx#this_type
		| New t -> string_to_type(t)
		| Operation (left, op, right) ->
			(match op with
			| Plus | Minus | Divided | Times | Mod -> get_type left ctx
			| _ -> TBool)
    	| InstanceOf (expr, str) -> TBool
    	| Cast (str, expr) -> string_to_type str
    	| SOperation (op, expr) -> get_type expr ctx
    	| ExpressionBlock (l) -> get_expr_list_type l ctx 
    	| FunctionCall (f, _) -> ctx#returntype_of_function(f)
		| If (_, l) -> 
			let t = get_expr_list_type l ctx in
			if t != TVoid then
				raise (GrammarError "Type of if must be void")
			else
				t
		| IfElse (_, l1, l2) ->
			let t1 = get_expr_list_type l1 ctx in
			let t2 = get_expr_list_type l2 ctx in
			if t1 == t2 then
				t1
			else 
				raise (GrammarError "Both types of if / else must be the same")
		) in (Hashtbl.add metatable node {_type = t}; t)

and get_expr_list_type l ctx = match l with
	| [] -> TVoid
	| l -> get_type (last_of_list l) ctx

and get_ident_type id ctx = match id with
	| NamedIdent(x) -> ctx#type_of_var x
	| MemberVar(expr, x) -> ctx#type_of_membervar (get_type expr ctx) x

let ensure_type node ctx =
	match node.expr with
	| VarCreation(v, ex) -> let t1  = v._type and t2 = get_type ex ctx in
		if not (ctx#type_implicitly_castable t2 (string_to_type t1)) then raise (GrammarError("Expr of type " ^ (type_to_string t2) ^ " cannot be cast into " 
			^ t1 ^ " in creation of variable " ^ v.name))
	| Assign(id, ex) ->
		let t1  = get_ident_type id ctx and t2 = get_type ex ctx in
		if not (ctx#type_implicitly_castable t2 t1) then raise (GrammarError("Expr of type " ^ (type_to_string t2) ^ " cannot be cast into " 
			^ (type_to_string t1) ^ " in assignment of variable " ^ (ident_to_string id)))
	| Operation (left, op, right) ->
		let t1 = get_type left ctx and t2 = get_type right ctx in 
		(match op with
		| Plus | Minus | Divided | Times | Mod | Lesser | Greater | LesserEq | GreaterEq 
			-> if t1 != TInt or t2 != TInt then raise (GrammarError ("Both operands must be of type Int"))
		| And | Or -> if t1 != TBool or t2 != TBool then raise (GrammarError ("Both operands must be of type Bool"))
		| _ -> ())
	| Cast (str, expr) -> let t1 = string_to_type str in let t2 = get_type expr ctx in 
		if ctx#type_implicitly_castable t1 t2 or ctx#type_implicitly_castable t2 t1 then ()
		else raise (GrammarError ("Expr of type " ^ (type_to_string t2) ^ " cannot be cast into " ^ str))
	| SOperation (op, expr) -> 
		let t = get_type expr ctx in (
		match op with 
		| SPlus | SMinus -> if (t != TInt) then raise (GrammarError ("Operand of unary +/- needs to be of type Int, not of type " ^ (type_to_string t)));
		| SNot -> if (t != TBool) then raise (GrammarError ("Operand of unary ! needs to be of type Bool, not of type " ^ (type_to_string t)));
	) 
	| FunctionCall (f, args) -> 
		let TFunc(_, argst) = ctx#type_of_function(f) in 
		if List.length argst != List.length args then 
			raise (GrammarError ("Function " ^ (ident_to_string f) ^ " called with the wrong number of arguments"))
		else (
			let rec match_arg_type lt larg =
			if lt != [] then (
				if ctx#type_implicitly_castable (List.hd lt) (get_type (List.hd args) ctx) then match_arg_type (List.tl lt) (List.tl larg) else
				raise (GrammarError ("Function " ^ (ident_to_string f) ^ " called with the wrong type of arguments; expected " ^ 
									 (type_to_string (List.hd lt)) ^ " and got " ^ (type_to_string (get_type (List.hd args) ctx))))
			) else () in match_arg_type argst args
		)
	| If (test, _) ->
		if get_type test ctx != TBool then 
			raise (GrammarError "Type of test inside if must be a boolean");
		();
	| IfElse (test, _, _) ->
		if get_type test ctx != TBool then 
			raise (GrammarError "Type of test inside if must be a boolean");
		();
	| _ -> ()
