(* Travels the AST *)
open Expr

let rec gather_toplevel l ctx = match l with 
	| Class (x) :: tl -> ctx#add_class x; gather_toplevel tl ctx
	| Function(x) ::tl -> ctx#add_function x; gather_toplevel tl ctx
	| _ ::tl -> gather_toplevel tl ctx
	| [] -> ()

