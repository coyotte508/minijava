(* 
   File defining the AST types, as well as utilities such as the one to convernt an AST
   to a string
*)

type binop =
    | Plus | Minus | Divided | Times | Mod
    | Eq | Lesser | Greater | LesserEq | GreaterEq | NotEq
    | And | Or

type unop = 
    | SPlus | SMinus | SNot

type _attribute = {_type : string; name: string}

type attribute_or_method = | Attribute of var | Method of _method 
and _class = {name: string; parent: string; attributes: var list; methods: _method list}

and expression =
	| Assign of ident * expression
    | VarCreation of var
	| Int of int
    | Bool of bool
	| String of string
    | Null
	| Ident of ident
    | This
    | New of string
	| Unit (* Nothing, () *)
	| Operation of expression * binop * expression
    | InstanceOf of expression * string
    | Cast of string * expression
	| SOperation of unop * expression
	| ExpressionBlock of expression list
	| FunctionCall of ident * expression list
	| If of expression * expression list
	| IfElse of expression * expression list * expression list

and var = _attribute * expression

and ident =
    | NamedIdent of string 
    | MemberVar of expression * string

and _method = {return_type: string; name: string;  arguments: _attribute list; body: expression list}

type class_or_expr = 
	| Class of _class
	| Expression of expression
	| Function of _method

let op_to_string = function
    | Plus -> "+"
    | Minus -> "-"
    | Divided -> "/"
    | Times -> "*"
    | Mod -> "%"
    | Eq -> "=="
    | Lesser -> "<"
    | Greater -> ">"
    | LesserEq -> "<="
    | GreaterEq -> ">="
    | NotEq -> "!="
    | And -> "and"
    | Or -> "or"

let unop_to_string = function
    | SPlus -> "+"
    | SMinus -> "-"
    | SNot -> "!"

let dattr_to_string = function
	| {_type=t; name=n} -> t ^ " " ^ n

let rec class_to_string = function |
	Class({name=name; parent=parent; attributes=attrs; methods=meths}) ->
		let str_list = ["class " ^ name ^ " inherits " ^ parent ^ " {"] @ (List.map var_to_string attrs) 
        @ (List.map method_to_string meths)  @ ["}"] in
		String.concat "\n" str_list

and ident_to_string = function
    | NamedIdent s -> s
    | MemberVar (e, s) -> "("^(dexpr_to_string e)^")."^s 

and var_to_string = function 
    | (a, Null) -> "var " ^ (dattr_to_string a) ^ ";"
    | (a, expr) ->  "varassign " ^ (dattr_to_string a) ^ " = (" ^ (dexpr_to_string expr) ^ ")"

and dexpr_to_string expr = 
	let rec body_to_string body = "{\n" ^ (String.concat "\n" (List.map (function x -> (dexpr_to_string x) ^ ";") body)) ^ "\n}"
	in match expr with
        | VarCreation c -> var_to_string c
		| Assign (id,expr) -> (ident_to_string id) ^ " = (" ^ (dexpr_to_string expr) ^ ")"
		| Int x -> string_of_int x
        | Bool x -> string_of_bool x
		| String s -> s
		| Ident s -> ident_to_string s
        | New s -> "new " ^ s ^ "()"
		| Unit -> "(Empty)"
        | This -> "<this>"
        | Null -> "<null>"
        | Cast (_class, expr) -> "(("^(dexpr_to_string expr)^") -> " ^ _class ^ ")"
        | InstanceOf (e1, cl) -> "(" ^ (dexpr_to_string e1) ^ ") is of " ^ cl
		| Operation (e1, op, e2) -> "(" ^ (dexpr_to_string e1) ^ ") " ^ (op_to_string op) ^ " (" ^ (dexpr_to_string e2) ^ ")"
		| SOperation (op, expr) -> (unop_to_string op) ^ (dexpr_to_string expr)
		| ExpressionBlock body -> (body_to_string body)
		| FunctionCall (name, args) -> "call '" ^ (ident_to_string name) ^ "' with args: [" ^
			(String.concat ", " (List.map dexpr_to_string args)) ^ "]"
        | If (cond, body) -> "if (" ^ (dexpr_to_string cond) ^ ") " ^ (body_to_string body)
        | IfElse (cond, body, elsebody) -> "if (" ^ (dexpr_to_string cond) ^ ") " ^ (body_to_string body) ^ " else " ^ (body_to_string elsebody)

and method_to_string = function
    | {return_type = return_type; name = name; arguments = arguments; body = body} 
		-> "function " ^ return_type ^ " named " ^ name ^ " (" ^  (String.concat ", " (List.map dattr_to_string arguments)) ^ ") {\n"
			^ (String.concat "\n" (List.map (function x -> (dexpr_to_string x) ^ ";") body))
			^ "\n}"

let expr_to_string = function 
	| Class cl -> class_to_string (Class cl)
	| Expression expr -> dexpr_to_string(expr) ^ ";"
	| Function m -> method_to_string m

let exprs_to_string = function |
 	l -> String.concat "\n" (List.map expr_to_string l)