%{
	open Expr
	open Exceptions

	let rec getattrs = function 
	| [] -> []
	| (Attribute a)::t -> a::(getattrs t)
	| h::t -> getattrs t

	let rec getmeths = function 
	| [] -> []
	| (Method a)::t -> a::(getmeths t)
	| h::t -> getmeths t
%}

%token EOF SEMICOLON ASSIGN COMMA
%token CLASS EXTENDS NEW THIS DOT NULL INSTANCEOF
%token IF ELSE AND OR
%token LPAR RPAR LCURL RCURL
%token PLUS MINUS TIMES DIVIDED MOD NOT
%token EQ LESSER LESSEREQ GREATER GREATEREQ NOTEQ
%token <string> LIDENT UIDENT STRING
%token <int> INT
%token <bool> BOOL

(* %left SEMICOLON (* x = 1+1; y=3+1; -> of course the statements in each branch have priority *)*)
%left ASSIGN (* a = 1 || 2 -> of course 1 || 2 has priority *)
%left OR
%left AND
%left EQ LESSER LESSEREQ GREATER GREATEREQ NOTEQ (* 1+1 > 2 is (1+1) > 2 *)
%left INSTANCEOF (* 1+1 instanceof Int => (1+1) instanceof Int *)
%left PLUS MINUS
%left MOD (* 2*2 % 3*2 is (2*2)%(3*2). 2+2%3 is 2+(2%3) *)
%left TIMES DIVIDED
%right SOP
%left DOT
%right CAST (* (A) b.x is ((A)b).x *)

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
| m=_function {Function m}
_class:
| CLASS name=UIDENT LCURL attrs=attribute_or_method* RCURL { Class {name=name; attributes=(getattrs attrs); methods=(getmeths attrs); parent="Object"} }
| CLASS name=UIDENT EXTENDS parent=UIDENT LCURL attrs=attribute_or_method* RCURL { Class {name=name; attributes=(getattrs attrs); methods=(getmeths attrs); parent=parent} }
_function:
| _type=UIDENT name=LIDENT LPAR args=arglist RPAR LCURL body=expr* RCURL { {name=name; return_type=_type; arguments=args; body=body} }
arglist:
| {[]}
| _type=UIDENT name=LIDENT { [{_type=_type; name=name}] }
| _type=UIDENT name=LIDENT COMMA args=arglist { {_type=_type; name=name} :: args }
expr:
| v=var {VarCreation v}
| i=condition {i}
| e=blexpr SEMICOLON {e}
var:
| _type=UIDENT name=LIDENT SEMICOLON { ({_type = _type; name=name}, Null) }
| _type=UIDENT name=LIDENT ASSIGN e=expr { ({_type = _type; name=name}, e)}
condition:
| IF LPAR cond=blexpr RPAR LCURL body=expr* RCURL { If(cond, body) }
| IF LPAR cond=blexpr RPAR LCURL body=expr* RCURL ELSE LCURL elsebody=expr* RCURL { IfElse(cond, body, elsebody) }
blexpr: /* bottom-level expression */
| NEW _class=UIDENT LPAR RPAR      {New(_class) }
| LPAR _class=UIDENT RPAR e=blexpr %prec CAST {Cast(_class, e) }
| MINUS e=blexpr %prec SOP         {SOperation(SMinus, e)} 
| PLUS e=blexpr %prec SOP          {SOperation(SPlus, e)}
| NOT e=blexpr %prec SOP           {SOperation(SNot, e)} 
| e1=blexpr INSTANCEOF cl=UIDENT   {InstanceOf(e1, cl)}
| e1=blexpr op=binop e2=blexpr     {Operation(e1, op, e2)}
| a=assign                         {a}
| id=ident                         {Ident id}
| THIS                             {This}
| NULL                             {Null}
| v=INT                            {Int v}
| v=STRING                         {String v}
| v=BOOL                           {Bool v}
| LPAR e=blexpr RPAR               {e}
| LPAR ex=expr* RPAR               {ExpressionBlock(ex)}
| name=ident LPAR args=callargslist RPAR {FunctionCall(name, args)}
callargslist:
| {[]}
| bl=blexpr { [bl] }
| bl=blexpr COMMA args=callargslist { bl :: args }
assign:
| name=ident ASSIGN e=blexpr { Assign (name, e) }
ident:
| obj=blexpr DOT mvar=LIDENT       {MemberVar(obj, mvar)}
| id=LIDENT                        {NamedIdent id}
attribute_or_method:
| a=var { Attribute a }
| m=_function { Method m }
%inline binop:
| MINUS     { Minus }
| PLUS      { Plus }
| TIMES     { Times }
| DIVIDED   { Divided }
| GREATER   { Greater }
| GREATEREQ { GreaterEq }
| LESSER    { Lesser }
| LESSEREQ  { LesserEq }
| EQ        { Eq }
| NOTEQ     { NotEq }
| MOD       { Mod }
| AND       { And }
| OR        { Or }
%%