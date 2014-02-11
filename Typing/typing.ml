open Expr 
open Exceptions

type node_type =
	| TInt
	| TBool
	| TString
	| TObject
	| TVoid
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
	| s -> TClass (s)

let rec type_to_string = function 
	| TBool -> "Bool"
	| TInt -> "Int"
	| TObject -> "Object"
	| TString -> "String"
	| TVoid -> "Void"
	| TClass s -> s
	| TFunc(t1, l) -> (type_to_string t1) ^ "(" ^ (String.concat ", " (List.map type_to_string l)) ^ ")"

let rec get_type node ctx =
	try
		let x = Hashtbl.find metatable node in 
		x._type
	with Not_found ->
		let t = (match node.expr with
		| Null -> TObject
		| Bool (b) -> TBool
		| Int (i) -> TInt
		| String (s) -> TString
		| Unit -> TVoid
		| VarCreation(v, _) -> ctx#add_var v; string_to_type(v._type)
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
    	| FunctionCall (f, _) -> ctx#type_of_function(f)
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
