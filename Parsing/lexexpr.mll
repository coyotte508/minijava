{
	open Parser
	open Location
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

rule nexttoken = parse 
  space+ { nexttoken lexbuf }
| newline { incr_line lexbuf; nexttoken lexbuf }
| eof { EOF }
| "class" { CLASS }
| "{" { LCURL }
| "}" { RCURL }
| ";" { SEMICOLON }
| "=" { ASSIGN }
| uident { UIDENT (Lexing.lexeme lexbuf) }
| lident { LIDENT (Lexing.lexeme lexbuf) }
| integer { INT (int_of_string (Lexing.lexeme lexbuf)) }
| string { STRING (Lexing.lexeme lexbuf) }
| _ as c { raise (Failure ("Unrecognized character: '" ^ (String.make 1 c) ^ "'")) }