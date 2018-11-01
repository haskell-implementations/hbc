module
#include "../misc/misc.t"
#include "types_t.t"
#include "ttype.t"
#include "id.t"
#include "constr_t.t"
#define String (List Char)
export eqTexpr;

rec eqConstr (Cconstr s t ti n _ tbl) (Cconstr s' t' ti' n' _ tbl') = 
               let (ts,bs) = split tbl in 
	       let (ts',bs') = split tbl' in 
		  s=s' & eqtype t t' & n = n' & And (map2 eqtype ts ts') 
		   -- I do not check ti and bs TIRED
and eqAtype (mkcons i _ tbl f) (mkcons i' _ tbl' f')  = 
               let (ts,bs,_) = split3 tbl in -- XXX
	       let (ts',bs',_) = split3 tbl' in 
                  eqid i i' & And (map2 eqtype ts ts') & f = f'
and eqgram _ _ = true -- extend later
and eqProd _ _ = true -- extend later
and eqLex _ _ = true -- extend later
and eqTeinfo _ _ = true -- extend later
and eqQual _ _ = true -- extend later
and eqFixid _ _ = true -- extend later
and eqImpid _ _ = true -- extend later
and eqExpid _ _ = true -- extend later
and eqOption _ _ = true -- extend later


and eqTexprseq ((e1,e2).es) ((e1',e2').es') =
		  eqTexpr e1 e1' & eqTexpr e2 e2' & eqTexprseq es es'
||  eqTexprseq [] [] = true
	  
and eqTexpr (mkap e1 e2) (mkap e1' e2') = eqTexpr e1 e1' & eqTexpr e2 e2'
||  eqTexpr (mklam e1 e2) (mklam e1' e2') = eqTexpr e1 e1' & eqTexpr e2 e2'
||  eqTexpr (mkcase e ees) (mkcase e' ees') = eqTexpr e e' & eqTexprseq ees ees'
||  eqTexpr (mkletv b e) (mkletv b' e') = eqBind b b' & eqTexpr e e'
||  eqTexpr (mkident i) (mkident i') = eqid i i'
||  eqTexpr (mkmodule mid fixes imps (Some exps) b) (mkmodule mid' fixes' imps' (Some exps') b')=
		eqid mid mid' & And (map2 eqFixid fixes fixes') &
	      	And (map2 eqImpid imps imps') &
		And (map2 eqExpid exps exps') & eqBind b b'
||  eqTexpr (mkconst c) (mkconst c') = c = c'
||  eqTexpr (mkcfunction b i) (mkcfunction b' i') = b=b' & eqid i i'
||  eqTexpr (mkbrack g lex) (mkbrack g' lex') = 
		  eqgram g g' & And (map2 eqLex lex lex')
||  eqTexpr (mkerror s) (mkerror s') = s = s'
||  eqTexpr (mkas i e) (mkas i' e') = eqid i i' & eqTexpr e e'
||  eqTexpr (mkcondp e1 e2) (mkcondp e1' e2') = eqTexpr e1 e1' & eqTexpr e2 e2'
||  eqTexpr (mklazyp e) (mklazyp e') = eqTexpr e e'
||  eqTexpr (mkconstr c es) (mkconstr c' es') =
		 eqConstr c c' & And (map2 eqTexpr es es')
||  eqTexpr (mkfailmatch n) (mkfailmatch n') = n=n'
||  eqTexpr (mkinfo ti e) (mkinfo ti' e') = eqTeinfo ti ti' & eqTexpr e e'
||  eqTexpr (mklistf n es) (mklistf n' es') = n=n' & And (map2 eqTexpr es es')
||  eqTexpr (mklistg e qs) (mklistg e' qs') = 
		 eqTexpr e e' & And (map2 eqQual qs qs')
||  eqTexpr (mkwhere ees b) (mkwhere ees' b') = And (map2 eq2T ees ees') & eqBind b b'
||  eqTexpr (mkhmodule i opei imps fixs b) (mkhmodule i' opei' imps' fixs' b')                 = true
||  eqTexpr _ _ = false

and eq2T (e1,e2) (e1',e2') = eqTexpr e1 e1' & eqTexpr e2 e2'

and eqBind (mkbtype t ats Oids iso) (mkbtype t' ats' Oids' iso') = 
	       eqtype t t' & And (map2 eqAtype ats ats') & eqOption Oids Oids' & iso = iso'
||  eqBind (mkbview t ot ats b) (mkbview t' ot' ats' b') = 
	       eqtype t t' & eqtype ot ot' & And (map2 eqAtype ats ats') & eqBind b b'
||  eqBind (mkbctype t prods) (mkbctype t' prods') =
	       eqtype t t' & And (map2 eqProd prods prods')
||  eqBind (mkbpat ees) (mkbpat ees') = eqTexprseq ees ees'
||  eqBind (mkbmulti e1 e2) (mkbmulti e1' e2') = 
	       eqTexpr e1 e1' & eqTexpr e2 e2'
||  eqBind (mkband b1 b2) (mkband b1' b2') = 
	       eqBind b1 b1' & eqBind b2 b2'
||  eqBind (mkbrec b) (mkbrec b') = eqBind b b'
||  eqBind (mkberror s) (mkberror s') = s=s'
||  eqBind (mkblocal b1 b2) (mkblocal b1' b2') = 
	       eqBind b1 b1' & eqBind b2 b2'
||  eqBind (mkbnull) (mkbnull) = true
||  eqBind (mkbpragma p) (mkbpragma p') = fail "eqBind pragma"
||  eqBind (mkbsyn t1 t2) (mkbsyn t1' t2') = eqtype t1 t1' & eqtype t2 t2'

end -- module
