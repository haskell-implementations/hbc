module -- constr
--
-- Convert constants to constructors and
--     constructor identifiers to constructors.
--
#include "../expr/id_t.t"
#include "../expr/constr_t.t"
#include "../expr/ttype_t.t"
#include "../expr/einfo_t.t"
#include "../expr/types_t.t"
#include "../expr/id.t"
#include "../expr/ttype.t"
#include "../expr/tinfo.t"
#include "../expr/constrfun.t"
#include "../rename/renameutil.t"
#include "../rename/renametype.t"
#include "../misc/misc.t"
#include "cutil.t"
#include "misc.t"
export constrtr;
-- Do constructor transformations
constrtr e u = (con true e u
where rec
    apl e l = revitlist (\a.\f.mkap f a) l e
and newid n = mkident (mknewid "AA" n)
and conapl n ii l u = 
    let ids = for u (u+n-1) newid in
    (reduce mklam (mkconstr (idtoconstr ii) (l @ ids)) ids, u+n)

and apc f l (mkap g a) u = let (a', u1) = con f a u in apc f (a'.l) g u1
||  apc f l (mkident (ii as mkid _ _ (idi_constr _ _ tyl _ _ _ vw) _)) u =
	if ~f | vw = None then
                let n = length tyl - length l in
		if n = 0 then
		    (mkconstr (idtoconstr ii) l, u)
		else if n < 0 then
		    (mkerror ("[35] Constructor with too many arguments: "@oprid ii), u)
		else -- n > 0
		    if f then
			-- Convert application to a lambda expression
			conapl n ii l u
		    else
			(mkerror ("[36] Constructor with too few arguments in pattern: "@oprid ii), u)
	else
	    (mkerror ("[115] View constructor in expression "@oprid ii), u)

-- MKRESTR
||  apc f l e u = let (e', u1) = con f e u in (apl e' l, u1)

and con f e u =
	case e in
	   mkap e1 e2 : let (a, u1) = con f e2 u in apc f [a] e1 u1
	|| mklam i e : Uap (mklam i) (con f e u)
	|| mkcase e pbs :
	       let (e', u1) = con f e u in
	       let (pbs', u2) = Umap conpb pbs u1 in
	       (mkcase e' pbs', u2)
	|| mkletv b e : 
	       let (b', u1) = conb b u in
	       let (e', u2) = con f e u1 in
	       (mkletv b' e', u2)
	|| mkident (ii as mkid _ _ (idi_constr _ _ [] _ _ _ vw) _) :
            if ~f | vw = None then
		(mkconstr (idtoconstr ii) [], u)
            else
                (mkerror ("[115] View constructor in expression "@oprid ii), u)
	|| mkident (ii as mkid _ _ (idi_constr _ _ l _ _ _ _) _) :
	        conapl (length l) ii [] u
	|| mkident _ : (e, u)
	|| mkmodule i fs is es b : let (b', u1) = conb b u in (mkmodule i fs is es b', u1)
	|| mkconst (cint i) : (mkint i, u)
	|| mkconst (cchar c) : (mkchar c, u)
	|| mkconst (cstring s) : (convstr s, u)
	|| mkconst (cfloat s) : (mkconstr (mkdfloat s) [], u)
	|| mkconst (cinteger s) : (mkconstr (mkinteger s) [], u)
	|| mkconst (crational s) : (mkconstr (mkrational s) [], u)
        || mkbrack gram lexl :      -- For conctypes 
	     let newgram = 
                     (map (\(tt1,gl).(tt1, map (\ (cid id,prod).
					             ccon(idtoconstr id),prod
				               )
		                               gl
				     )	
		 	  )
			  gram
		     ) 
	     in
	       let (u2,newlexl) = 
		     mapstate (\u.\x.
			      case x in
                                (mklt c) : (u,mklt c)
                              ||(mkltint c) : (u,mkltint c)
                              ||(mkltid c) : (u,mkltid c)
                              ||(mkltsym c) : (u,mkltsym c)
                              ||(mkunq ex) : let (exn,u1) = con f ex u
					     in
						   (u1,mkunq exn)
			      end
			      )
			      u
			      lexl
	       in 
		  (mkbrack newgram newlexl,u2)
	|| mkerror _ : (e, u)
	|| mkcfunction _ _ : (e, u)
	|| mkas i e : Uap (mkas i) (con f e u)
	|| mkcondp p c : 
	       let (p', u1) = con false p u in
	       let (c', u2) = con true c u1 in
	       (mkcondp p' c', u2)
	|| mklazyp p : Uap mklazyp (con f p u)
	|| mkinfo t e : Uap (mkinfo t) (con f e u)
        || mkconstr c es : -- is introduced e.g. in genderiv
               let (es', u') = Umap (con f) es u
               in  (mkconstr c es', u')
	|| mkwhere ges b : 
	        let (ges', u') = Umap (\(g,e).\u.let (g', u') = con f g u in 
                                                 let (e', u'') = con f e u' in ((g', e'), u'')) ges u in
                let (b', u'') = conb b u' in
	        (mkwhere ges' b', u'')
	-- mkfailmatch cannot occur
	end
and
    conb b u =
	case b in
	   mkbtype _ _ _ _ : (b, u)
        || mkbctype _ _ : (b,u)
	|| mkbpat ps : Uap mkbpat (Umap conpb ps u)
	|| mkbmulti p e : let ((np, ne), u1) = conpb (p,e) u in (mkbmulti np ne, u1)
	|| mkband b1 b2 : 
	       let (b1', u1) = conb b1 u in
	       let (b2', u2) = conb b2 u1 in
	       (mkband b1' b2', u2)
	|| mkbrec b : Uap mkbrec (conb b u)
	|| mkberror _ : (b, u)
	|| mkblocal b1 b2 :
	       let (b1', u1) = conb b1 u in
	       let (b2', u2) = conb b2 u1 in
	       (mkblocal b1' b2', u2)
	|| mkbnull : (b, u)
	|| mkbpragma _ : (b, u)
	-- All others should be gone by now.
	end
and
    conpb (e1, e2) u =
        let (e1', u1) = con false e1 u in
        let (e2', u2) = con true e2 u1 in
	((chkpat e1', e2'), u2)
)
end
