(* 
   File defining the AST types, as well as utilities such as the one to convernt an AST
   to a string
*)

type _attribute = {_type : string; name: string}
type attribute = | Attribute of _attribute 
type _class = {name: string; attributes: attribute list}

type binop =
	| Plus | Minus | Divided | Times

type unop = 
	| SPlus | SMinus 

type expression =
	| Var of _attribute
	| Assign of string * expression
	| VarAssign of _attribute * expression
	| Int of int
	| String of string
	| Name of string
	| Unit (* Nothing, () *)
	| Operation of expression * binop * expression
	| SOperation of unop * expression
	(* Two expressions, to execute in order *)
	| ExpressionBlock of expression list
	| FunctionCall of string * expression list
	| If of expression * expression list
	| IfElse of expression * expression list * expression list

type _method = {return_type: string; name: string;  arguments: _attribute list; body: expression list}

type class_or_expr = 
	| Class of _class
	| Expression of expression
	| Method of _method

let dattr_to_string = function
	| {_type=t; name=n} -> t ^ " " ^ n

let attr_to_string = function 
	| Attribute a -> "\t" ^ (dattr_to_string a) ^ ";"

let class_to_string = function |
	Class({name=name; attributes=attrs}) ->
		let str_list = ["class " ^ name ^ "{"] @ (List.map attr_to_string attrs) @ ["}"] in
		String.concat "\n" str_list

let op_to_string = function
	| Plus -> "+"
	| Minus -> "-"
	| Divided -> "/"
	| Times -> "*"

let unop_to_string = function
	| SPlus -> "+"
	| SMinus -> "-"

let rec dexpr_to_string expr = 
	let rec body_to_string body = "{\n" ^ (String.concat "\n" (List.map (function x -> (dexpr_to_string x) ^ ";") body)) ^ "\n}"
	in match expr with
		| Var a -> "var " ^ (dattr_to_string a) ^ ";"
		| Assign (str,expr) -> str ^ " = (" ^ (dexpr_to_string expr) ^ ")"
		| VarAssign (a, expr) ->  "varassign " ^ (dattr_to_string a) ^ " = (" ^ (dexpr_to_string expr) ^ ")"
		| Int x -> string_of_int x
		| String s -> s
		| Name s -> s
		| Unit -> "(Empty)"
		| Operation (e1, op, e2) -> "(" ^ (dexpr_to_string e1) ^ ") " ^ (op_to_string op) ^ " (" ^ (dexpr_to_string e2) ^ ")"
		| SOperation (op, expr) -> (unop_to_string op) ^ (dexpr_to_string expr)
		| ExpressionBlock body -> (body_to_string body)
		| FunctionCall (name, args) -> "call '" ^ name ^ "' with args: [" ^
			(String.concat ", " (List.map dexpr_to_string args)) ^ "]"
        | If (cond, body) -> "if (" ^ (dexpr_to_string cond) ^ ") " ^ (body_to_string body)
        | IfElse (cond, body, elsebody) -> "if (" ^ (dexpr_to_string cond) ^ ") " ^ (body_to_string body) ^ " else " ^ (body_to_string elsebody)

let method_to_string = function
    | {return_type = return_type; name = name; arguments = arguments; body = body} 
		-> "function " ^ return_type ^ " named " ^ name ^ " (" ^  (String.concat ", " (List.map dattr_to_string arguments)) ^ ") {\n"
			^ (String.concat "\n" (List.map (function x -> (dexpr_to_string x) ^ ";") body))
			^ "\n}"

let expr_to_string = function 
	| Class cl -> class_to_string (Class cl)
	| Expression expr -> dexpr_to_string(expr) ^ ";"
	| Method m -> method_to_string m

let exprs_to_string = function |
 	l -> String.concat "\n" (List.map expr_to_string l)