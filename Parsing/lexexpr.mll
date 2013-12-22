{
	open Parser
}

let letter = ['a'-'z' 'A'-'Z']
let uletter = ['A'-'Z']
let lletter = ['a'-'z']
let digit = ['0'-'9']
let uident = uletter (letter | digit | '_')*
let lident = lletter (letter | digit | '_')*
let space = [' ' '\t' '\n']

rule nexttoken = parse 
  space+ { nexttoken lexbuf }
| eof { EOF }
| "class" { CLASS }
| "{" { LCURL }
| "}" { RCURL }
| uident { UIDENT (Lexing.lexeme lexbuf) }
| lident { LIDENT (Lexing.lexeme lexbuf) }