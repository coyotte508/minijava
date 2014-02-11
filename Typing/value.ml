open Expr

type value = 
	| NullValue 
	| IntValue of int 
	| StringValue of string 
	| BoolValue of bool
	| NoValue
	| ClassValue of classvalue
	| FunctionValue of functiondef

(* Can be extended to add info about location, ... *)
and scopedata = { _type: Typing.node_type ; mutable value : value}

and parent =
	| NoParent
	| Parent of scopedata

and classvalue = {attributes: (string, scopedata) Hashtbl.t; parent: parent; actual_type : Typing.node_type}

and functiondef = { return_type: Typing.node_type; arguments: _attribute list; body: expression list }

type attributedef = { _type: Typing.node_type; init_val: expression }
type classdata = { attributedefs: (string, attributedef) Hashtbl.t;
					methoddefs:   (string, functiondef) Hashtbl.t;
					parent: string
					}

let make_int x = {_type = Typing.TInt; value = IntValue(x)}
let make_bool x = {_type = Typing.TBool; value = BoolValue(x)}
let make_string x = {_type = Typing.TString; value = StringValue(x)}
let make_null t = {_type = t; value = NullValue}
let void_value = {_type = Typing.TVoid;  value = NoValue}

let same_value a b = 
	try
		let StringValue (s1) = a and StringValue (s2) = b in (String.compare s1 s2) == 0
	with Match_failure _ ->
		a == b