(*Compiles a file. Take in entry a lex buffer, gets it ast, and then does typing analysis on it *)

open Expr

let execute lexbuf verbose = 
	let exprs = (Parser.compile Lexexpr.nexttoken lexbuf) in
	(
		if (verbose) then print_endline (Expr.exprs_to_string exprs);
		let ctx = new Context.context in
		Traveler.gather_toplevel exprs ctx;
		Traveler.visit_tree Typing.get_type exprs ctx;
		Traveler.visit_tree Typing.ensure_type exprs ctx;
		ctx#ensure_return_types;
		(*print_endline "typing todo"*)
	)