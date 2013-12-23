%{
	open Expr
	open Exceptions
%}

%token CLASS EOF LCURL RCURL SEMICOLON ASSIGN
%token <string> LIDENT UIDENT STRING
%token <int> INT

(* Entry point *)
%start compile
%type < Expr.class_or_expr list > compile

%%
compile:
| c=class_or_expr* EOF {c}
| error { Location.print (Location.symbol_loc $startpos $endpos); raise SyntaxError }
class_or_expr:
| c=_class { c}
| e=expr {Expression e}
_class:
| CLASS name=UIDENT LCURL attrs=attribute* RCURL { Class {name=name; attributes=attrs} }
expr:
| v=var {v}
| e=blexpr SEMICOLON {e}
var:
| _type=UIDENT name=LIDENT SEMICOLON { Var {_type = _type; name=name} }
| _type=UIDENT name=LIDENT ASSIGN e=expr { VarAssign ({_type = _type; name=name}, e)}
blexpr:
| a=assign {a}
| v=INT {Int v}
| v=STRING {String v}
assign:
| name=LIDENT ASSIGN e=blexpr { Assign (name, e) }
attribute:
| _type=UIDENT name=LIDENT SEMICOLON { Attribute {_type = _type; name=name} }
%%