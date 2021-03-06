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
let string = "\"" [^'"']* "\"" | "'" [^'\'']* "'"
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
| "<=" { LESSEREQ }
| ">=" { GREATEREQ }
| "==" { EQ }
| "!=" { NOTEQ }
| ">" { GREATER }
| "<" { LESSER }
| "=" { ASSIGN }
| "!" { NOT }
| "+" { PLUS }
| "-" { MINUS }
| "*" { TIMES }
| "/" { DIVIDED } 
| "," { COMMA }
| "." { DOT }
| "%" { MOD }
| "if" { IF }
| "else" { ELSE }
| "&&" { AND }
| "||" { OR }
| "true" { BOOL true}
| "false" { BOOL false }
| "extends" { EXTENDS }
| "new" { NEW }
| "this" { THIS }
| "null" { NULL }
| "instanceof" { INSTANCEOF }
| uident { UIDENT (Lexing.lexeme lexbuf) }
| lident { LIDENT (Lexing.lexeme lexbuf) }
| integer { INT (int_of_string (Lexing.lexeme lexbuf)) }
| string as s { STRING (String.sub s 1 ((String.length s) - 2)) }
| _ as c { raise (Failure ("Unrecognized character: '" ^ (String.make 1 c) ^ "'")) }