import type Expr = Eap Expr Expr + Elam Id Expr + Ecase Expr (List (#3 Constr (List Id) Expr)) Expr + Elet Bool (List (Id # Expr)) Expr + Evar Id + Econstr Constr (List Expr) + Emodule Id (List Expid) (List (List (Id # Expr))) + Efailmatch Int + Eidapl Id (List Expr) + Elaml (List Id) Expr + Einfo Teinfo Expr + Ecfunction Bool Id;