(* 
   File defining the AST types, as well as utilities such as the one to convernt an AST
   to a string
*)

type _attribute = {_type : string; name: string}
type attribute = | Attribute of _attribute 
type _class = {name: string; attributes: attribute list}

type binop =
	| Plus | Minus | Divided | Times
	
type expression =
	| Var of _attribute
	| Assign of string * expression
	| VarAssign of _attribute * expression
	| Int of int
	| String of string
	| Combination of expression * binop * expression

type class_or_expr = 
	| Class of _class
	| Expression of expression

let attr_to_string = function |
	Attribute ({_type=t; name=n}) -> "\t" ^ t ^ " " ^ n ^ ";"

let class_to_string = function |
	Class({name=name; attributes=attrs}) ->
		let str_list = ["class " ^ name ^ "{"] @ (List.map attr_to_string attrs) @ ["}"] in
		String.concat "\n" str_list

let expr_to_string = function 
	| Class cl -> class_to_string (Class cl)
	| Expression expr -> ""

let exprs_to_string = function |
 	l -> String.concat "\n" (List.map expr_to_string l)