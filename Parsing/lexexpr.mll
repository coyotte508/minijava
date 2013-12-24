{
	open Parser
	open Location
	open String

	let rec repeat n f = match n with
	| 0 -> (function x -> ())
	| 1 -> f
	| n -> (function x -> (f x; repeat (n-1) f x;))
	let count_lines str =
		let rec count_char str c index =  
			try
				let i = String.index_from str index c
				in 1 + (count_char str c (i+1))
			with Not_found -> 0
		in count_char str '\n' 0
}

let letter = ['a'-'z' 'A'-'Z']
let uletter = ['A'-'Z']
let lletter = ['a'-'z']
let digit = ['0'-'9']
let uident = uletter (letter | digit | '_')*
let lident = lletter (letter | digit | '_')*
let newline = ("\r\n" | '\r' | '\n')
let space = [' ' '\t']
let integer = digit+
let string = "\"" [^'"']* "\""
let linecomment = "//" [^'\r' '\n']*
let multilinecomment = "/*" ([^'*'] | ('*' [^'/']))* "*/"

rule nexttoken = parse 
  space+ { nexttoken lexbuf }
| newline { incr_line lexbuf; nexttoken lexbuf }
| eof { EOF }
| linecomment {nexttoken lexbuf}
| multilinecomment { repeat (count_lines (Lexing.lexeme lexbuf)) incr_line lexbuf; nexttoken lexbuf}
| "class" { CLASS }
| "{" { LCURL }
| "}" { RCURL }
| "(" { LPAR }
| ")" { RPAR }
| ";" { SEMICOLON }
| "=" { ASSIGN }
| "+" { PLUS }
| "-" { MINUS }
| "*" { TIMES }
| "/" { DIVIDED } 
| "," { COMMA }
| "if" { IF }
| "else" { ELSE }
| uident { UIDENT (Lexing.lexeme lexbuf) }
| lident { LIDENT (Lexing.lexeme lexbuf) }
| integer { INT (int_of_string (Lexing.lexeme lexbuf)) }
| string { STRING (Lexing.lexeme lexbuf) }
| _ as c { raise (Failure ("Unrecognized character: '" ^ (String.make 1 c) ^ "'")) }