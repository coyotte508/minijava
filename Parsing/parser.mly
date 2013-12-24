%{
	open Expr
	open Exceptions
%}

%token CLASS EOF SEMICOLON ASSIGN COMMA IF ELSE
%token LPAR RPAR LCURL RCURL
%token PLUS MINUS TIMES DIVIDED
%token <string> LIDENT UIDENT STRING
%token <int> INT

(* %left SEMICOLON (* x = 1+1; y=3+1; -> of course the statements in each branch have priority *)*)
%left ASSIGN (* a = 1+1 -> of course 1+1 has priority *)
%left PLUS MINUS
%left TIMES DIVIDED
%right SPLUS SMINUS

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
| m=_method {m}
_class:
| CLASS name=UIDENT LCURL attrs=attribute* RCURL { Class {name=name; attributes=attrs} }
_method:
| _type=UIDENT name=LIDENT LPAR args=arglist RPAR LCURL body=expr* RCURL { Method {name=name; return_type=_type; arguments=args; body=body} }
arglist:
| {[]}
| _type=UIDENT name=LIDENT { [{_type=_type; name=name}] }
| _type=UIDENT name=LIDENT COMMA args=arglist { {_type=_type; name=name} :: args }
expr:
| v=var {v}
| i=condition {i}
| e=blexpr SEMICOLON {e}
var:
| _type=UIDENT name=LIDENT SEMICOLON { Var {_type = _type; name=name} }
| _type=UIDENT name=LIDENT ASSIGN e=expr { VarAssign ({_type = _type; name=name}, e)}
condition:
| IF LPAR cond=blexpr RPAR LCURL body=expr* RCURL { If(cond, body) }
| IF LPAR cond=blexpr RPAR LCURL body=expr* RCURL ELSE LCURL elsebody=expr* RCURL { IfElse(cond, body, elsebody) }
blexpr: /* bottom-level expression */
| MINUS e=blexpr %prec SMINUS      {SOperation(SMinus, e)} 
| PLUS e=blexpr %prec SPLUS        {SOperation(SPlus, e)} 
| e1=blexpr op=binop e2=blexpr     {Operation(e1, op, e2)}
| a=assign                         {a}
| name=LIDENT                      {Name name}
| v=INT                            {Int v}
| v=STRING                         {String v}
| LPAR e=blexpr RPAR               {e}
| LPAR ex=expr* RPAR               {ExpressionBlock(ex)}
| name=LIDENT LPAR args=callargslist RPAR {FunctionCall(name, args)}
callargslist:
| {[]}
| bl=blexpr { [bl] }
| bl=blexpr COMMA args=callargslist { bl :: args }
assign:
| name=LIDENT ASSIGN e=blexpr { Assign (name, e) }
attribute:
| _type=UIDENT name=LIDENT SEMICOLON { Attribute {_type = _type; name=name} }
%inline binop:
| MINUS    { Minus }
| PLUS     { Plus }
| TIMES    { Times }
| DIVIDED  { Divided }
%%