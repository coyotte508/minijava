(*Compiles a file. Take in entry a lex buffer, gets it ast, and then does typing analysis on it *)

let execute lexbuf verbose = 
	let exprs = (Parser.compile Lexexpr.nexttoken lexbuf) in
	(
		print_endline (Expr.exprs_to_string exprs);
		let ctx = new Context.context in
		Traveler.gather_toplevel exprs ctx;
		print_endline "typing todo"
	)