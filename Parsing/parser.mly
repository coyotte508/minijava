%{
	open Expr
%}

%token CLASS EOF LCURL RCURL
%token <string> LIDENT UIDENT

(* Entry point *)
%start compile
%type < Expr.expression > compile

%%
compile:
| c=_class EOF {c}
_class:
| CLASS d=UIDENT LCURL RCURL { Class(d) }
%%