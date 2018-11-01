module -- errors
--
-- handle errors
--
#include "id_t.t"
#include "id.t"
#include "constr_t.t"
#include "ttype_t.t"
#include "ttype.t"
#include "einfo_t.t"
#include "types_t.t"
#include "pprint.t"
#include "pragma_t.t"
#include "impexp_t.t"
#include "../transform/misc.t"

export errors, merror, findloc;
rec
    merror e msg = 
	case ee e in
	   [] : mkerror (msg @ "\n" @ ppr e @ "\n")
	|| _  : e	-- don't nest errors
	end
and errors errtab t = map (findloc errtab " in definition of ") (eetop t)
and /*eetop (mkap (mkerror msg) _) = [[msg]]		-- Act of desperation in rename
||*/  eetop t = ee t	-- HIDES reason for export error.
and
    ee (mkap e1 e2) = ee e1 @ ee e2
||  ee (mklam i e) = ee e
||  ee (mkcase e pbl) = ee e @ concmap ep pbl
||  ee (mkletv b e) = eb b @ ee e
||  ee (mkident i) = []
||  ee (mkmodule _ _ imp _ b) = eb b
||  ee (mkconst c) = []
||  ee (mkbrack _ _) = []               --  for conctypes
||  ee (mkerror emsg) = [[emsg]]
||  ee (mkas _ e) = ee e
||  ee (mkcondp p c) = ee p @ ee c
||  ee (mklazyp p) = ee p
||  ee (mkconstr _ el) = concmap ee el
||  ee (mkfailmatch _) = []
||  ee (mkinfo (restr _ t) e) = et t @ ee e
||  ee (mkinfo t e) = ee e
||  ee (mklistf _ es) = concmap ee es
||  ee (mklistg e qs) = ee e @ concmap eq qs
||  ee (mkwhere ges b) = concmap (\(g,e).ee g @ ee e) ges @ eb b
||  ee (mkcfunction _ _) = []
||  ee (mkdo s) = es s
||  ee (mkrecord c ies _) = ee c @ concmap (ee o snd) ies
||  ee e = fail ("no match in ee: " @ppr e)

and es (mksexp e) = ee e
||  es (mksexpstmt e s) = ee e @ es s
||  es (mkslet b s) = eb b @ es s
||  es (mksbind p e s) = ee p @ ee e @ es s

and eq (mkqfilter e) = ee e
||  eq (mkqgen p e) = ee p @ ee e
||  eq (mkqlet b) = eb b

and eb (mkbtype t ats _ _) = addnamei (tname t) (et t @ eats ats)
||  eb (mkbview t ot ats b) = addnamei (tname t) (et t @ et ot @ eats ats @ eb b)
||  eb (mkbctype t _)  = et t             -- for conctypes
||  eb (mkbpat pbl) = addnamei (leftmostid (fst (hd pbl))) (concmap ep pbl)
||  eb (mkband b1 b2) = eb b1 @ eb b2
||  eb (mkbnull) = []
||  eb (mkblocal b1 b2) = eb b1 @ eb b2
||  eb (mkbrec b) = eb b
||  eb (mkbmulti p e) = ee p @ ee e
||  eb (mkberror emsg) = [[emsg]]
||  eb (mkbsyn t1 t2) = addnamei (tname t1) (et t1 @ et t2)
||  eb (mkbclass (c as mkcdecl aas a) b) = addnamei (clsname c) (concmap ea (a.aas) @ eb b)
||  eb (mkbinstance (mkidecl aas _ t _) b _) = concmap ea aas @ eb b @ concmap et t
||  eb (mkbdefault ts) = concmap et ts
||  eb (mkbsign is t) = et t
||  eb (mkbpragma p) = epr p

and ep (e, p) = ee e @ ee p

and epr (Pspecialize i ts) = concmap (et o fst) ts
||  epr (Pspecinst t) = et t

and et (mktcons _ ts) = concmap et ts
||  et (mktap _ ts) = concmap et ts
||  et (mktvar _) = []
||  et (mkterror emsg) = [[emsg]]
||  et (mktcontype aas t) = concmap ea aas @ et t

and ea (mkaerror emsg) = [[emsg]]
||  ea _ = []

and eats ats = (concmap eat ats
		where eat (mkcons _ (_,_,k) tbs _) = 
		    concmap ea k @
		    concmap (\(t,_,_).et t) tbs)

and eimp (mkimpid _ t _ ots) = et t @ eimpo ots
||  eimp (mkimptype _ _ _) = []
||  eimp (mkimpeqtype _ _ ats _ _ _) = eats ats
||  eimp (mkimpview _ _ _ ats) = eats ats
and eimpo None = []
||  eimpo (Some ts) = concmap (et o fst) ts

and addnamei i l = map (\es.idtostr i.es) l

and findloc errtab _ [e] = fmt (fname errtab) 0 e
||  findloc errtab d ("Pmain".es) = findloc errtab d es
||  findloc errtab d ("Pinteractive".es) = findloc errtab d es
||  findloc errtab d (es as (f._)) =
    let (msg.ids) = reverse es in
    let msg = mix (msg.map drop_ ids) d in
    let (n, l) = assocdef f errtab (fname errtab, 0) in
    fmt n l msg
and fname ((_,(n,_))._) = n
||  fname _ = ""	
and drop_ ('_'.s) = s
||  drop_ s = s
and fmt "" 0 msg = msg
||  fmt n l msg = "\""@n@"\", line "@itos l@", "@msg
end
