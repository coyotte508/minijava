(* Travels the AST *)
open Expr

(* Gather general information, aka classes & functions in the code *)
let rec gather_toplevel l ctx = match l with 
	| Class (x) :: tl -> ctx#add_class x; gather_toplevel tl ctx
	| Function(x) ::tl -> ctx#add_function x; gather_toplevel tl ctx
	| _ ::tl -> gather_toplevel tl ctx
	| [] -> ()

(* Applies a function to every expression of the tree *)
let rec apply_fun_toplevel f l ctx = match l with
	| Class (x) :: tl -> apply_fun_class f x ctx; apply_fun_toplevel f tl ctx
	| Function (x) :: tl -> apply_fun_function f x ctx; apply_fun_toplevel f tl ctx
	| Expression (x) :: tl -> apply_fun_expr f x ctx; apply_fun_toplevel f tl ctx
	| [] -> ()

and apply_fun_class f c ctx =
	ctx#dive_into_class c.name;
	List.map (fun (x,y) -> f y ctx) c.attributes;
	List.map (fun m -> apply_fun_function f m ctx) c.methods; 
	ctx#exit_class

and apply_fun_function f m ctx = 
	ctx#dive_into_function m;
	List.map (fun e -> apply_fun_expr f e ctx) m.body;
	ctx#exit_function

and apply_fun_expr f expr ctx = 
	try 
		(match expr.expr with
		| VarCreation(v, ex) -> apply_fun_expr f ex ctx; ()
		| Assign(id, ex) -> apply_fun_expr f ex ctx; ()
		| Operation (left, op, right) -> apply_fun_expr f left ctx; apply_fun_expr f right ctx; ()
		| InstanceOf (ex, str) -> apply_fun_expr f ex ctx; ()
		| Cast (str, ex) -> apply_fun_expr f ex ctx; ()
		| SOperation (op, ex) -> apply_fun_expr f ex ctx; ()
		| ExpressionBlock (l) -> ctx#dive_into_scope; List.map (fun x -> apply_fun_expr f x ctx) l; ctx#exit_scope
		| FunctionCall (_, args) -> List.map (fun x -> apply_fun_expr f x ctx) args; ()
		| If (test, l) ->
			apply_fun_expr f test ctx; ctx#dive_into_scope; List.map (fun x -> apply_fun_expr f x ctx) l; ctx#exit_scope
		| IfElse (test, l1, l2) ->
			apply_fun_expr f test ctx; 
			ctx#dive_into_scope; List.map (fun x -> apply_fun_expr f x ctx) l1; ctx#exit_scope;
			ctx#dive_into_scope; List.map (fun x -> apply_fun_expr f x ctx) l2; ctx#exit_scope
		| Ident (MemberVar(ex, s)) -> apply_fun_expr f ex ctx; ()
		| _ -> ()); f expr ctx
	with e ->
		print_endline ("Corresponding code: " ^ (dexpr_to_string expr));
		raise e 

(* Applies a function to every expr of the tree, but doesn't change it *)
let visit_tree = apply_fun_toplevel

(* Applies a function to every expr of the tree, and replace them by the return value of the function.
	To return a brand new expr, for example "2": `(Int 2)`

	Allows to do programmation by aspect, cf Serge Guelton's lessons! *)
let transform_tree f l ctx = 
	apply_fun_toplevel (fun x y -> x.expr <- (f x y); x) l ctx