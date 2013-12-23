%{
	open Expr
	open Exceptions
%}

%token CLASS EOF LCURL RCURL SEMICOLON
%token <string> LIDENT UIDENT

(* Entry point *)
%start compile
%type < Expr.expression > compile

%%
compile:
| c=_class EOF {c}
| error { Location.print (Location.symbol_loc $startpos $endpos); raise SyntaxError }
_class:
| CLASS name=UIDENT LCURL attrs=attribute* RCURL { Class {name=name; attributes=attrs} }
attribute:
| _type=UIDENT name=LIDENT SEMICOLON { Attribute {_type = _type; name=name} }
%%