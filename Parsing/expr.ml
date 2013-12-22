(* 
   File defining the AST types, as well as utilities such as the one to convernt an AST
   to a string
*)

type expression = | Class of string

let expr_to_string = function |
	Class(ident) -> "class " ^ ident ^ "{}"