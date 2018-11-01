module
export Expr;
rec
type Expr =
	  Eap Expr Expr		
	+ Elam Id Expr
	+ Ecase Expr (List (Constr#(List Id)#Expr)) Expr
	+ Elet Bool (List (Id#Expr)) Expr
	+ Evar Id
	+ Econstr Constr (List Expr)
	+ Emodule Id (List Expid) (List (List (Id#Expr)))		-- name, exported ids, and defs
	+ Efailmatch Int
	-- variants used in lambda lifting
	+ Eidapl Id (List Expr)
	+ Elaml (List Id) Expr
	-- special nodes
	+ Einfo Teinfo Expr
        + Ecfunction Bool Id
end
