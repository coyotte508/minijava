(*Compiles a file. Take in entry a lex buffer, gets it ast, and then does typing analysis on it *)

let execute lexbuf verbose = 
  print_endline (Expr.exprs_to_string (Parser.compile Lexexpr.nexttoken lexbuf));
  print_endline "typing todo"
