module
#ifdef BWM
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/constr_t.t"
#include "../expr/constrfun.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Eprint.t"
#include "../misc/util.t"
export bwmconv, Bexpr, show_Bmodule, show_Bexpr, Bmodule, Bid, bvar2id, eqbv;
rec type Bexpr = 
        Bvar Bid +
	Bapply Bid (List Bexpr) + 
	Bcase Bexpr (List Bid) (List (Constr#(List Bid)#Bexpr)) +
	Blet (List (Bid#Bexpr)) Bexpr
#if 0
	Bnaked Int +
	Bbox Bexpr 
#endif
and type Bid = Bvstack Int Id + Bvheap Int Id + Bvglob Id + Bvconstr Int Int Constr + Bvalures
and type Bmodule == List (List (Bid#(List Bid)#Bexpr))
and bvar2id (Bvstack _ i) = i
||  bvar2id (Bvheap _ i) = i
||  bvar2id (Bvglob i) = i
||  bvar2id (Bvconstr _ _ _) = mkid (-100) "?constr" idi_udef Noorigname
||  bvar2id (Bvalures) = mkid (-101) "?alu" idi_udef Noorigname
and eqbv v1 v2 = eqid (bvar2id v1) (bvar2id v2)
and bvar2str (Bvalures) = "ALU"
||  bvar2str (Bvconstr _ _ c) = prc c
||  bvar2str v = oprid (bvar2id v)
and show_Bmodule bm = concmap (concmap show_Bfun) bm
and show_Bfun (f,is,e) = "\n-----\n"@bvar2str f @ " = \\" @ mix (map bvar2str is) " " @ "." @ show_Bexpr e @ "\n"
and show_Bid (Bvstack s i) = prid i@"{S"@itos s@"}"
||  show_Bid (Bvheap  h i) = prid i@"{H"@itos h@"}"
||  show_Bid (Bvglob    i) = prid i
||  show_Bid (Bvconstr k n c) = prc c@"{C"@itos k@"_"@itos n@"}"
||  show_Bid (Bvalures   ) = "ALU"
and show_Bexpr (Bvar v)       = show_Bid v
||  show_Bexpr (Bapply v es)  = "("@show_Bid v@" "@mix (map show_Bexpr es) " " @ ")"
||  show_Bexpr (Blet ies e) = "\nlet "@mix (map pries ies) "\nand "@"\nin  "@show_Bexpr e
||  show_Bexpr (Bcase e ts cies) = "\ncase "@show_Bexpr e@" in "@prtstk ts@"\n   "@mix (map prcie cies) "\n|| "@"\nend"
#if 0
||  show_Bexpr (Bnaked k) = "#"@itos k
||  show_Bexpr (Bbox e) = "(BOX "@show_Bexpr e@")"
#endif
and prtstk [] = ""
||  prtstk is = "{TOSTACK "@mix (map show_Bid is)", "@" }"
and prcie (c,is,e) = prc c@" "@mix (map bvar2str is) " "@" : "@show_Bexpr e
and pries (i,e) = bvar2str i@" = "@show_Bexpr e
and prc (c as Cconstr _ _ _ _ _ _) & (isstring c) = '"'.cname c@"\""
||  prc (Cconstr ('_'.name) _ _ _ _ _) = pn name
||  prc (Cconstr name _ _ _ _ _) = pn name
and pn (s as c._) = if isalpha c | isdigit c | c = '\'' then s else "("@s@")"

and bwmconv :: Expr -> Bmodule
and bwmconv (Emodule _ _ dss) = map (map convf) dss
and convf (i, Elaml is e) = (convi i, map2 Bvstack (from 0) is, convc [] e)
and convc ls (Elet _ ies e) = Blet (map convie ies) (convc (map (convi o fst) ies @ ls) e)
||  convc ls (Ecase e cies _) = 
    let cies' = map convcie cies in
    let us = reduce (unioneq eqbv) [] (map (\ (_,_,e).Bgetids e) cies') in
    Bcase (conve e) (intereq eqbv ls us) cies'
||  convc ls (Einfo _ e) = convc ls e
||  convc _  e = conve e
and conve (Eidapl i [])  = Bvar (convi i)
||  conve (Eidapl i es)  = Bapply (convi i) (map conve es)
||  conve (Econstr c []) = Bvar (Bvconstr (cno c) 0 c)
||  conve (Econstr c es) = Bapply (Bvconstr (cno c) (length es) c) (map conve es)
||  conve (Elet _ ies e) = Blet (map convie ies) (conve e)
||  conve (Einfo _ e) = conve e
||  conve e = fail ("No match in conve "@pr e)
and convie (i,e) = (convi i, conve e)
and convcie (c,is,e) = (c, map convi is, convc [] e)
and convi i = if id_is_global i | id_is_predef i then Bvglob i else Bvstack (-1) i -- temporary

and Bgetids (Bvar v) = [v]
||  Bgetids (Bapply v es) = reduce (unioneq eqbv) [v] (map Bgetids es)
||  Bgetids (Bcase e _ _) = Bgetids e
||  Bgetids (Blet ies e) = reduce (unioneq eqbv) (Bgetids e) (map (Bgetids o snd) ies)
#if 0
||  Bgetids (Bnaked _) = []
||  Bgetids (Bbox e) = Bgetids e
#endif
#else
export ;
dummy=0
#endif
end
