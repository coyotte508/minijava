{
	open Parser
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit | '_')*

let space = [' ' '\t' '\n']

rule nexttoken = parse 
  space+ { nexttoken lexbuf }
| eof { EOF }
| "class" { CLASS }
| ident { IDENT (Lexing.lexeme lexbuf) }