module
#include "id_t.t"
#include "id.t"
#include "ttype_t.t"
#include "ttype.t"
#include "types_t.t"
#include "einfo_t.t"
#include "annot.t"
#include "impexp_t.t"
#include "booltree.t"
#include "einfo.t"
#include "pragma_t.t"
#include "../misc/misc.t"

export Read, readimpid, readexpr, readbinding, readimplist, readinterface, readstring, readimport, readlist;
rec
    splittab l = splittab' l []
and splittab' ('\t'.cs) r = (cs, reverse r)
||  splittab' (c.cs) r = splittab' cs (c.r)

#if 0
and skipspace (' '.l) = skipspace l
 || skipspace ('\t'.l) = skipspace l
 || skipspace ('\n'.l) = skipspace l
 || skipspace l = l
#else
#define skipspace
#endif

and readimplist l = readlist readimpid l

and
    readlist f ll =
	case skipspace ll in
	   'N'.l :
		(l, [])
	|| 'L'.l :
		let! (l1, h) = f l in
		let! (l2, t) = readlist f l1 in
		(l2, h.t)
	|| s : fail ("No match in readlist: " @ head 200 ll)
	end
and
    readstring ll =
	case skipspace ll in
	   '#'.l : splittab l
	end
and
    readid ll =
	let! (l1, s) = readstring ll in
	(l1, mkids (unidecode s))
and
    readop ll =
	let! (l1, i) = readstring ll in
	let! (l2, f) = readstring l1 in
	(l2, (i, f))

and readge l =
        let! (l1, g) = readexpr l in
	let! (l2, e) = readexpr l1 in
        (l2, (g, e))

and
    readexpr ll =
	case skipspace ll in
	   'a'.l :
		let! (l1, e1) = readexpr l in
		let! (l2, e2) = readexpr l1 in
		(l2, mkap e1 e2)
	|| 'l'.l :
		let! (l1, i) = readexpr l in
		let! (l2, e) = readexpr l1 in 
		(l2, mklam i e)
	|| 'c'.l :
		let! (l1, e) = readexpr l in
		let! (l2, bl) = readlist readpbinding l1 in
		(l2, mkcase e bl)
	|| 'e'.l :
		let! (l1, b) = readbinding l in
		let! (l2, e) = readexpr l1 in
		(l2, mkletv b e) 
	|| 'i'.l :
		let! (l1, i) = readid l in
		(l1, mkident i) 
	|| 'm'.l :
		let! (l0, opl) = readlist readfix l in
		let! (l1, impl) = readlist readimpid l0 in
		let! (l2, exp) = readoption (readlist readid) l1 in
		let! (l3, b) = readbinding l2 in
		(l3, mkmodule (mkids "_LML") opl impl (oapply (map mkexpid) exp) b) 
	|| 'h'.l :
		let! (l0, modid) = readid l in
		let! (l1, expids) = readoption (readlist readexpid) l0 in
		let! (l2, imps) = readlist readimport l1 in
		let! (l3, fixes) = readlist readfix l2 in
		let! (l4, b) = readbinding l3 in
		(l4, mkhmodule modid expids imps fixes b)
	|| 'I'.l :
		let! (l1, s) = readstring l in
		(l1, mkconst (cint (stoi s))) 
	|| 'E'.l :
		let! (l1, t) = readlist readlex l in
				(l1,mkbrack [] t)
	|| 'C'.l :
		let! (l1, s) = readstring l in
		(l1, mkconst (cchar (stochar s))
			where stochar l = chr (stoi l)) 
	|| 'S'.l :
		let! (l1, s) = readstring l in
		(l1, mkconst (cstring s)) 
	|| 'F'.l :
	        let! (l1, s) = readstring l in
		(l1, mkconst (cfloat s))
	|| 'J'.l :
	        let! (l1, s) = readstring l in
		(l1, mkconst (cinteger (fixI s)))
	|| 'Q'.l :
	        let! (l1, s) = readstring l in
		(l1, mkconst (crational s))
	|| 's'.l :
		let! (l1, i) = readid l in
		let! (l2, e) = readexpr l1 in
		(l2, mkas i e) 
	|| 'o'.l :
		let! (l1,e1) = readexpr l in
		let! (l2,e2) = readexpr l1 in
		(l2, mkcondp e1 e2) 
	|| 'R'.l :
		let! (l1,e) = readexpr l in
		let! (l2,t) = readttype l1 in
		(l2, mkinfo (restr [] t) e) 
	|| '|'.l :
		let! (l1,e) = readexpr l in
		let! (l2,a) = readstring l1 in
                if a = "" then (l2, e) 
		          else (l2, mkinfo (parse_annot a) e)
	|| '1'.l :
	        let! (l1,s) = readstring l in
		let! (l2,es) = readlist readexpr l1 in
		(l2, mklistf (stoi s) es)
	|| '2'.l :
		let! (l1,e) = readexpr l in
		let! (l2,qs) = readlist readqual l1 in
		(l2, mklistg e qs)
	|| 'Z'.l :
	        let! (l1, p) = readexpr l in
		(l1, mklazyp p)
	|| 'W'.l :
		let! (l1, ges) = readlist readge l in
		let! (l2, b) = readbinding l1 in
		(l2, mkwhere ges b)
	|| 'D'.l :
	        let! (l1, s) = readstmt l in
		(l1, mkdo s)
	|| 'r'.l :
	        let! (l1, c) = readexpr l in
		let! (l2, fs) = readlist (read2 readid readexpr) l1 in
		(l2, mkrecord c fs [])
	|| r : fail ("No match in readexpr on:"@r)
	end

and
    readstmt ll =
        case skipspace ll in
	    'e'.l :
	         let! (l1, e) = readexpr l in
		 (l1, mksexp e)
	||  's'.l :
	         let! (l1, e) = readexpr l in
	         let! (l2, s) = readstmt l1 in
		 (l2, mksexpstmt e s)
	||  'l'.l :
	         let! (l1, b) = readbinding l in
	         let! (l2, s) = readstmt l1 in
		 (l2, mkslet b s)
	||  'b'.l :
	         let! (l1, p) = readexpr l in
	         let! (l2, e) = readexpr l1 in
	         let! (l3, s) = readstmt l2 in
		 (l3, mksbind p e s)
        ||  r : fail ("No match in readstmt on:"@r)
        end
and
    readexpid ll =
        case skipspace ll in
	    '3'.l :
		 let! (l1, i) = readid l in
		 (l1, mkexpid i)
	||  '4'.l :
		 let! (l1, i) = readid l in
		 (l1, mkexpidall i)
	||  '5'.l :
		 let! (l1, i) = readid l in
		 (l1, mkexpidmodall i)
	||  '6'.l :
		 let! (l1, i) = readid l in
		 let! (l2, is) = readlist readid l1 in
		 (l2, mkexpidsome i is)
        end

and
    readimport ll =
        case skipspace ll in
	    '7'.l :
		 let! (l1, id) = readid l in
		 let! (l2, imps) = readlist readimpid l1 in
		 let! (l3, fixs) = readlist readfix l2 in
		 let! (l4, ents) = readlist readimpid l3 in
		 let! (l5, (b, is)) = readimpspec l4 in
		 let! (l6, rens) = readlist readrename l5 in
		 let! (l7, qual) = readstring l6 in
		 let! (l8, ns) = readlist readid l7 in
		 (l8, mkimport id imps fixs ents b is rens (qual="1") (case ns in [] : None || (n . _) : Some n end))
	end

and
    readinterface ll =
        case skipspace ll in
	    '^'.l :
		 let! (l1, id) = readid l in
		 let! (l2, imps) = readlist readimpid l1 in
		 let! (l3, fixs) = readlist readfix l2 in
		 let! (l4, ents) = readlist readimpid l3 in
		 (l4, mkimport id imps fixs ents false [] [] false None)
	|| l : fail ("no match in readinterface: "@ l)
	end
and
    readfix ll = 
        case skipspace ll in
	    '0'.l :
		 let! (l1, ids) = readlist readid l in
		 let! (l2, ass) = readstring l1 in
		 let! (l3, prec) = readstring l2 in
		 (l3, mkfixid ids (select (ord (hd ass) - ord '0' +1) [Infix; InfixL; InfixR; FPrefix; FPostfix; (\x.Nonfix)] (stoi prec)))
	end
and
    readimpspec ll = 
        case skipspace ll in
	    '8'.l :
		 let! (l1, b) = readstring l in
		 let! (l2, is) = readlist readexpid l1 in
		 (l2, (b="1", is))
	end

and
    readoption f ll =
        case skipspace ll in
	    '`'.l :
		(l, None)
	||  '='.l :
		let! (l1, x) = f l in
		(l1, Some x)
	end
and
    readtinfo ll =
        case skipspace ll in
	    '`'.l :
		(l, (0, false))
	||  '<'.l :
		let! (l1, x) = readstring l in
		let! (l2, y) = readstring l1 in
		(l2, (stoi x, y="1"))
	end
and
    readrename ll = 
        case skipspace ll in
	    '9'.l :
		 let! (l1, src) = readid l in
		 let! (l2, dst) = readid l1 in
		 (l2, (src, dst))
	end

and
    readqual ll =
        case skipspace ll in
	    'g'.l :
		let! (l1, p) = readexpr l in
		let! (l2, e) = readexpr l1 in
		(l2, mkqgen p e)
	||  'f'.l :
		let! (l1, e) = readexpr l in
		(l1, mkqfilter e)
	||  'l'.l :
		let! (l1, b) = readbinding l in
		(l1, mkqlet b)
	end

and
    readlex ll =
	case skipspace ll in
	   'U'.c.l  :
		(l, mklt(c))
        || 'D'.l : 
                let! (l1,n) = readstring l in 
                (l1,mkltint (stoi n))
        || 'G'.l :
                let! (l1,i) = readstring l in 
                (l1,mkltid i)
        || 'B'.l :
                let! (l1,s) = readstring l in 
                (l1,mkltsym s) 
	|| 'q'.l :
		let! (l1,e) = readexpr l in
		(l1, mkunq e)
	||  x :
		fail ("readlex: " @ x)
        end
and
    readcgs ll =
	case skipspace ll in
	   'M'.c.l  :
		(l, mkct c)
        || 'L'.l :
                let! (l1, n) = readstring l in 
                (l1,mkctint (stoi n))
        || 'R'.l :
                let! (l1, i) = readstring l in 
                (l1,mkctid i)   
        || 'N'.l :
                let! (l1, s) = readstring l in 
                (l1,mkctsym s)              
	|| 'n'.l :
		let! (l1,t) = readttype l in
		(l1, mkcnt t)
	|| '&'.l :
		let! (l1,t) = readttype l in
		let! (l2,al) = readlist readcgs l1 in
		(l2,mklist1 t al 0)
	|| '%'.l :
		let! (l1,t) = readttype l in
		let! (l2,al) = readlist readcgs l1 in
		(l2,mklist0 t al)
	
	end

and
    readassoc ll =
        case skipspace ll in
           'L'.l : (l,aleftassoc)
        || 'R'.l : (l,arightassoc)
        || 'N'.l : (l,anonassoc)
        || 'B'.l : (l,abothassoc)
        end 
                  
and
    readprod ll =
	case skipspace ll in
	   'D'.l :
		let! (l1,t) = readlist readcgs l in
                let! (l2,p) = readstring l1 in
		let! (l3,ass) = readassoc l2 in 
		(l3,mknormal t (stoi p, ass))
	|| 'G'.l :
		let! (l1,t) = readlist readcgs l in
                let! (l2,p) = readstring l1 in
		let! (l3,ass) = readassoc l2 in 
		(l3,mkforget t (stoi p, ass))
	end

and
    readbinding ll =
	case skipspace ll in
	   't'.l :
		let! (l1, t) = readttype l in
		let! (l2, al) = readlist readatype l1 in
		let! (l3, ds) = readoption (readlist readid) l2 in
		(l3, mkbtype t al ds false) 
	|| 'v'.l :
		let! (l1, t) = readttype l in
		let! (l2, ot) = readttype l1 in
		let! (l3, al) = readlist readatype l2 in
		let! (l4, b) = readbinding l3 in
		(l4, mkbview t ot al b) 
	|| 'x'.l :
		let! (l1, t) = readttype l in
		let! (l2, al) = readlist readatype l1 in
		let! (l3, ds) = readoption (readlist readid) l2 in
		(l3, mkbtype t al ds true)
        || 'b'.l :
		let! (l1, t) = readttype l in
		let! (l2, al) = readlist readprod l1 in
		(l2, mkbctype t al)
	|| 'p'.l :
		let! (l1, p) = readlist readpbinding l in
		(l1, mkbpat p) 
	|| 'A'.l :
		let! (l1, b1) = readbinding l in
		let! (l2, b2) = readbinding l1 in
		(l2, mkband b1 b2) 
	|| 'O'.l :
		let! (l1, b1) = readbinding l in
		let! (l2, b2) = readbinding l1 in
		(l2, mkblocal b1 b2) 
	|| 'r'.l :
		let! (l1, b) = readbinding l in
		(l1, mkbrec b) 
	|| 'z'.l :
	        let! (l1, t1) = readttype l in
		let! (l2, t2) = readttype l1 in
		(l2, mkbsyn t1 t2)
	|| ':'.l :
	        let! (l1, _, t) = readcdecl l in
		let! (l2, b) = readbinding l1 in
	        (l2, mkbclass t b)
	|| ';'.l :
	        (l, mkbnull)
	|| '\''.l :
	        let! (l1, ts) = readlist readttype l in
	        (l1, mkbdefault ts)
	|| '.'.l :
	        let! (l1, t) = readidecl l in
		let! (l2, b) = readbinding l1 in
	        (l2, mkbinstance t b None)
	|| '+'.l :
	        let! (l1, is) = readlist readid l in
		let! (l2, t) = readttype l1 in
	        (l2, mkbsign is t)
	|| '<'.l :
	        let readttypeval l =
		    let! (l1, t) = readttype l in
		    if hd (skipspace l1) = '#' then
			let! (l2, i) = readid l1 in
			(l2, (t, Some i))
		    else
			(l1, (t, None))
		in
	        let! (l1, i) = readid l in
		let! (l2, ts) = readlist readttypeval l1 in
	        (l2, mkbpragma (Pspecialize i ts))
	|| '>'.l :
	        let! (l1, t) = readttype l in
	        (l1, mkbpragma (Pspecinst t))
	end
and readcdecl l =
	        let! (l1, (k, t)) = readkttype l in
		(l1, k, mkcdecl (cpart t) (ty2as (tpart t)))
and readidecl l =
	        let! (l1, t) = readttype l in
		let! (mktcons ci ts) = tpart t in
		(l1, mkidecl (cpart t) ci ts "_")
and readkind ll =
        case skipspace ll in
	   '>'.l :
	       let! (l1, k1) = readkind l in
	       let! (l2, k2) = readkind l1 in
	       (l2, mkkarrow k1 k2)
	|| '*'.l :
	       (l, mkkground)
	|| l : fail ("no match in readkind: "@head 100 l)
	end
and
    readttype ll = 
	case skipspace ll in
	   'T'.l :
		let! (l1, i) = readid l in
		let! (l2, t) = readlist readttype l1 in
		(l2, mktcons i t) 
	|| 'A'.l :
		let! (l1, mktvar i) = readttype l in
		let! (l2, t) = readlist readttype l1 in
		(l2, mktap i t) 
	|| 'y'.l :
		let! (l1, s) = readstring l in
		(l1, mktvar (stoi s)) 
	|| 'C'.l :
		let! (l1, ts) = readlist readttype l in
		let! (l2, t) = readttype l1 in
		(l2, mktcontype (acconv ts) t)
	|| l : fail ("no match in readttype: "@l)
	end
and
    read2 r1 r2 ll =
	let! (l1, x1) = r1 ll in
	let! (l2, x2) = r2 l1 in
	(l2, (x1, x2))

and

    readatype ll =
	case skipspace ll in
	   '1'.l :
                let! (l0, c) = if hd (skipspace l) = '#' then (l, []) else let! (l', ts) = readlist readttype l in (l', acconv ts) in
		let! (l1, i) = readid l0 in
		let! (l2, t) = readlist readsttype l1 in
		let! (ts, flg) = flatconstrs t in
		(l2, mkcons i (all (\ (t,_,_).postyvar t) ts,[],abstyvarcx c) [(abstyvar t,b,x);;(t,b,x) <- ts] flg) 
	end
    and flatconstrs xs = let isYes (Yes _) = true || isYes _ = false in (concmap flatyn xs, all isYes xs & length xs > 0)
    and flatyn (Yes ists) = ists
    ||  flatyn (No tb) = [tb]
    and abstyvar (mktcons i ts) = mktcons i (map abstyvar ts)
    ||  abstyvar (mktvar n) = mktvar (abs n)
    ||  abstyvar (mktcontype c t) = mktcontype (abstyvarcx c) (abstyvar t)
    ||  abstyvar (mktap v ts) = mktap (abs v) (map abstyvar ts)
    and abstyvarcx k = map (\ (mkassert i vs) . mkassert i (map abs vs)) k
    and postyvar (mktcons _ ts) = all postyvar ts
    ||  postyvar (mktvar v) = v > 0
    ||  postyvar (mktcontype c t) = all (\ (mkassert _ vs) . all (\v.v>0) vs) c & postyvar t
    ||  postyvar (mktap v ts) = v > 0 & all postyvar ts
and

    readsttype ll =
        case skipspace ll in
	   '!'.l :
	   	let! (l1, t) = readttype l in
		(l1, No (t,true,None)) 
	|| 'S'.l :
	        let! (l1, ists) = readlist (read2 (readlist readid) readsttype) l in
		(l1, Yes [ (t,b,Some i) ;; (is, No (t,b,s)) <- ists ; i <- is ])
	|| l	 :
		let! (l1, t) = readttype l in
		(l1, No (t,false,None)) 
	end

and xreadlist f (cs as ('N'._)) = readlist f cs
||  xreadlist f (cs as ('L'._)) = readlist f cs
||  xreadlist f cs = (cs, [])

and readpstr l =
	let! (l1, x1) = readid l in
	let! (l2, (x2,_,_)) = readfinfo l1 in
	(l2, (x1,x2))

and readkttype ll =
	case skipspace ll in
	   'T'.l :
		let! (l1, i) = readid l in
		let! (l2, ts) = readlist readttype l1 in
		(l2, (mkkarrows (rept (length ts) mkkground) mkkground, mktcons i ts))
	|| 'K'.l :
		let! (l1, i) = readid l in
		let! (l2, k) = readkind l1 in
		let! (l3, ts) = readlist readttype l2 in
		(l3, (k, mktcons i ts))
	|| 'C'.l :
		let! (l1, ts) = readlist readttype l in
		let! (l2, (k, t)) = readkttype l1 in
		(l2, (k, mktcontype (acconv ts) t))
	|| l : fail ("no match in readkttype: "@l)
	end
and
    readimpid ll =
	case skipspace ll in
	   'f'.l :
		let! (l1, i) = readid l in
		let! (l2, t) = readttype l1 in
		let! (l3, (f, ts, ev)) = readfinfo l2 in
		(l3, mkimpid i t f (if null ts then None else Some ts))
	|| 'Y'.l :
		let! (l1, (k, t)) = readkttype l in
		let! (l2, ds) = readoption (readlist readid) l1 in
		let! (l3, nf) = readtinfo l2 in
		(l3, mkimptype k t nf) 
	|| '@'.l :
		let! (l1, (k, t)) = readkttype l in
		let! (l2, al) = readlist readatype l1 in
		let! (l3, ds) = readoption (readlist readid) l2 in
		(l3, mkimpeqtype k t al ds false true)
	|| 'W'.l :
		let! (l1, (k, t)) = readkttype l in
		let! (l2, tof) = readttype l1 in
		let! (l3, al) = readlist readatype l2 in
		(l3, mkimpview k t tof al)
	|| '/'.l :
		let! (l1, (k, t)) = readkttype l in
		let! (l2, al) = readlist readatype l1 in
		let! (l3, ds) = readoption (readlist readid) l2 in
		(l3, mkimpeqtype k t al ds true true)
	|| '{'.l :
	       let! (l1, i) = readid l in
	       let! (l2, exps) = readlist readexpid l1 in
	       let! (l3, rens) = readlist readrename l2 in
	       (l3, mkimpimport i exps rens)
        || '}'.l :
	       let! (l1, (k, src)) = readkttype l in
	       let! (l2, dst) = readttype l1 in
	       (l2, mkimpsyn k src dst)
	|| '['.l :
	       let! (l1, k, t) = readcdecl l in
	       let! (l2, b) = readbinding l1 in
	       let! (l3, xxs) = readlist readpstr l2 in
	       (l3, mkimpclass k t b xxs)
        || ']'.l :
	       let! (l1, mkidecl x y z _) = readidecl l in
	       let! (l2, d) = readstring l1 in
	       let! (l3, m) = readstring l2 in
	       let! (l4, xxs) = readlist readpstr l3 in
	       (l4, mkimpinstance (mkidecl x y z m) (d = "1") xxs)
	|| 'F'.l :
		let! (l1, is) = readlist readid l in
		let! (l2, t) = readttype l1 in
		let! (l3, (f, ts, ev)) = readfinfo l2 in
		(l3, mkimpids is t f)
	|| 'K'.l :
	        let! (l1, t) = readttype l in
		let! (l2, ps) = readlist readprod l1 in
		(l2, mkimpctype t ps)
	end

and
    readpbinding ll =
	case skipspace ll in
	   'd'.l :
		let! (l1, e1) = readexpr l in
		let! (l2, e2) = readexpr l1 in
		(l2, (e1, e2)) 
	end

and
    readfinfo ll =
	case skipspace ll in
	   '*'.l :
		let! (l1, _.s1) = readstring l in
		let! (l2, _.s2._) = readstring l1 in
		let! (l3, s3) = if hdishash l2 then readstring l2 else (l2, "-2") in
		(l3, (finfo (length s1) [] (TF2bt s1,btff) (stoi s3) None, [], "0"))
        || '_'.l :
	        let! (l1, il) = case skipspace l in
		                    '_'.ll : (ll, None)
				||  ll : let (x, e) = readexpr ll in (x, Some e)
				end in
		let! (l2, sst) = readstring l1 in
                let! (l3, es) = readlist readstring l2 in
                let! (l4, ars) = readstring l3 in
		let ar = stoi ars in
                let! (l5, fr) = readstring l4 in
		let! (l6, tfs) = readlist readttypeX l5 in
		let! (l7, ev) = readstring l6 in
                (l7, (finfo ar (map parseentry es) (parsestr sst) (stoi fr) il, tfs, ev))
	|| _ :
		(ll, (f_unk, [], "0"))
	end
and readttypeX ll =
	let! (l1, t) = readttype ll in
	let! (l2, (fi, _, _)) = readfinfo l1 in
--trace ("readttypeX "@head 30 ll@"\nret "@head 20 l1@"\nis "@prttype t, prfinfo fi "")
        (l2, (t, fi))
and parsestr sst = let (s,t) = splitat ',' sst in (xxxparsebt s bttt, parsebt t btff)
and hdishash ('#'._) = true
||  hdishash _ = false

and TF2bt xs = btands (conc (map2 (\n.\x.if x = 'F' then [] else [btors [btvar n]]) (from 0) xs))

and parseentry e = map parse1entry e
and parse1entry 'N' = (AInothing, APframe)
||  parse1entry 'E' = (AIeval,    APframe)
||  parse1entry 'U' = (AIunboxed, APframe)
||  parse1entry 'n' = (AInothing, APregister)
||  parse1entry 'e' = (AIeval,    APregister)
||  parse1entry 'u' = (AIunboxed, APregister)
and xxxparsebt "F" _ = bttt -- !!!
||  xxxparsebt s   _ = parsebt s bttt
and parsebt ""  d = d
||  parsebt "F" _ = btff
||  parsebt "T" _ = bttt
||  parsebt s _   = (btands (map por (choplist (splitat '&') s))
		 where por ('('.s) = btors (map (btvar o stoi) (choplist (splitat '|') (butlast s)))
                    || por s = btors[btvar (stoi s)])

and readerrmap ll =
    let (l1, func) = readstring ll in
    let (l2, file) = readstring l1 in
    let (l3, line) = readstring l2 in
    (l3, (func, (file, stoi line)))
and acconv c = map ty2as c
and Read l = 
    let (l1, e) = readexpr l in
    let (l2, errtab) = readlist readerrmap l1 in
    (e, errtab)

and fixI ('x'.s) = Iitos (stoIbase 16# s 0#) @ "I"
||  fixI ('o'.s) = Iitos (stoIbase  8# s 0#) @ "I"
||  fixI s = s
and stoIbase b (c.cs) r = stoIbase b cs (r *# b +# Int2Integer (digit c))
|| stoIbase b "" r = r
and digit c = 
    if '0' <= c & c <= '9' then ord c - ord '0'
    else if 'a' <= c & c <= 'f' then ord c - ord 'a' + 10
    else if 'A' <= c & c <= 'F' then ord c - ord 'A' + 10
    else fail "Bad digit in bignum"

and unidecode "" = ""
||  unidecode ('\\'.'u' .d1.d2.d3.d4.r) = 
     chr (fromhex [d1;d2;d3;d4] 0) . unidecode r
||  unidecode (c.r) = c . unidecode r

and fromhex "" a = a
||  fromhex (c.cs) a = fromhex cs (a * 16 + fromhex1 c)
and fromhex1 c = if isdigit c then ord c - ord '0' 
                 else if 'A' <= c & c <= 'F' then ord c - ord 'A' + 10
                 else if 'a' <= c & c <= 'f' then ord c - ord 'a' + 10
                 else fail "fromhex"
end
