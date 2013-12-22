%{
	open Expr
%}

%token CLASS EOF
%token <string> IDENT

(* Entry point *)
%start compile
%type < Expr.expression > compile

%%
compile:
| c=_class EOF {c}
_class:
| CLASS d=IDENT { Class(d) }
%%