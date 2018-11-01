module -- pprint
--
-- prettyprinter
--
#include "id_t.t"
#include "constr_t.t"
#include "ttype_t.t"
#include "einfo_t.t"
#include "types_t.t"
#include "../transform/misc.t"
#include "id.t"
#include "ttype.t"
#include "einfo.t"
#include "../misc/flags.t"
#include "../misc/misc.t"
#include "constrfun.t"
#include "../syntax/listgen.h"
#include "impexp.t"
#include "impexp_t.t"
#include "pragma_t.t"

export ppr, prdefg, prderiv, prprod, prass, prcgs, prps, prgram, prmkimport, prpragma;
rec
    ppr e = pr1 0 e @ "\n"
and
    primport (i as mkimport id imps fixs ents show exps rens qual asname) =
        if ImpDebug then
	    "import "@prmkimport i
	else
	    "import " @ oprid id @ "..."
and prmkimport (mkimport id imps fixs ents show exps rens qual asname) =
    prid id@" "@
    (if qual then "qualified " else "")@
    show_list primpid imps@" "@
    show_list prfix fixs@" "@
    show_list primpida ents@" "@
    show_bool show@" "@
    show_list prexpid exps@" "@
    show_list (show_pair (prid,prid)) rens@
    (case asname in None : "" || Some i : " as "@prid i end)

and primpida (mkimpid i t fi opt) = oprid i @ ": " @ prttype t @ prfinfo fi (prid i) @
	case opt in
	   None : ""
	|| Some tfs : "{# SPECIALIZE "@mix (map (\(t,f).prttype t @ prfinfo f (prid i)) tfs) ", " @ " #}"
	end
||  primpida i = primpid i
    
and prfix (mkfixid ids f) = prfixity f @ " " @ mix (map prid ids) ","
and prfixity (Infix p) = "infix "@itos p@" "
||  prfixity (InfixL p) = "infixl "@itos p@" "
||  prfixity (InfixR p) = "infixr "@itos p@" "
||  prfixity (FPrefix p) = "prefix "@itos p@" "
||  prfixity (FPostfix p) = "postfix "@itos p@" "
||  prfixity (Nonfix) = "nonfix "
||  prfixity Nofixity = ""
and
    issimple (mkident _) = true
 || issimple (mkconst _) = true
 || issimple (mkconstr _ []) = true
 || issimple _ = false
and ppr1 i e = if issimple e then pr1 i e else '(' . pr1 i e @ ")"
and prderiv None = ""
||  prderiv (Some is) = " deriving (" @ mix (map oprid is) "," @ ")"
and
    nli i = '\n' . space(4*i)
and
    prass aleftassoc  = " leftassoc "
 || prass arightassoc = " rightassoc "
 || prass anonassoc   = " nonassoc "
 || prass abothassoc  = " "

and 
    prprec p = if p>0 then show_int p else ""

and 
    prps ps = concmap (\(t,(p,ass)).prttype t@" "@prprec p@" "@prass ass@",") ps
and

    prcgs i (mkct c)      = [c]
 || prcgs i (mkctint n)   = show_int n
 || prcgs i (mkctid s)    = s
 || prcgs i (mkctsym s)   = s
 || prcgs i (mkcnt t) = "<" @ prttype t @ ">"
 || prcgs i (mklist1 t l _) = "{" @ prttype t @ concmap (prcgs i) l @ "...}+"
 || prcgs i (mklist0 t l) = "{" @ prttype t @ concmap (prcgs i) l @ "...}*"
 || prcgs i (mklistend n b) = "mklistend " @ show_int n @" " @ show_bool b
and
    prlex i (mklt c)      = [c]
 || prlex i (mkltint n)   = show_int n
 || prlex i (mkltid s)    = s
 || prlex i (mkltsym s)   = s
 || prlex i (mkunq e)   = "^(" @ pr1 i e @ ")"
 
and
    prprod i (mknormal l (p,ass)) = prprec p @ prass ass @
                                    "[|"  @ mix (map (prcgs i) l) " " @ "|]"
 || prprod i (mkforget l (p,ass)) = prprec p @ prass ass @ 
                                    "[||" @ mix (map (prcgs i) l) " " @ "||]"

and prpragma (Pspecialize i ts) = 
     let prttypev (t, None) = prttype t
     ||  prttypev (t, Some i) = prttype t @ " = " @ prid i
     in  "{-# SPECIALIZE "@prid i@" :: "@mix (map prttypev ts) ", " @ " #-}"
||  prpragma (Pspecinst t) = "{-# SPECIALIZE instance "@prttype t@" #-}"

and prgram g = (mix (map (pralts 0) g) ("\n and ")
                 where  pralts i (tt, altlist) =
		             prttype tt @ " ::= " @
		                   mix (map ((prprod i) o snd) altlist) " + "
               )
and optpr f None = ""
||  optpr f (Some x) = f x
and pratype (mkcons i (u,vs,cx) lt _) =
		    (if null vs then "" else if u then "forall " else "exists ")@
		    (if null cx then "" else "(" @ mix (map (prttype o as2ty) cx) ", " @ ") =>")@
		    mix (oprid i.map (\(t,b,_)."("@prttype t@")"@if b then "!" else "") lt) " "
and
    prdefg i d = (
	case d in
	   mkbtype t l od iso :
		(if iso then " newtype " else " type ") @ prttype t @ " = " @ mix (map pratype l) " + " @ prderiv od
	|| mkbview t ot l b :
		" view " @ prttype t @ " of " @ prttype ot @ " = " @ mix (map pratype l) " + " @ " where" @ nli (i+1) @ prdefg (i+1) b @ nli i @ "endview"
        || mkbctype t l :
		(" conctype " @ prttype t @ " = " @ mix (map prqdef l) " + "
					where rec prqdef l = prprod i l)

	|| mkbpat l :
		mix (map prbind l) (nli i @ "|| ")
	|| mkbmulti p e :
		prbind (p,e)
	|| mkband b1 b2 :
		(if isrec b1 then
			"(" @ prdefg i b1 @ ")"
		else
			prdefg i b1) @
		nli i @ "and " @ prdefg i b2
	|| mkblocal b1 b2  :
		"local " @ prdefg i b1 @ nli i @ " in " @ prdefg i b2 @ " end"
	|| mkbrec b :
		"rec " @ prdefg i b
	|| mkberror s :
		"ERROR " @ s
	|| mkbnull : "/* EMPTY */"
        || mkbpragma p : prpragma p
	|| mkbsyn t1 t2 : " type " @ prttype t1 @ " == " @ prttype t2
	|| mkbclass t b : " class " @ prcdecl t @ " where " @ nli (i+1) @ prdefg (i+1) b @ nli i @ "endclass"
	|| mkbinstance t b _ : " instance " @ pridecl t @ " where " @ nli (i+1) @ prdefg (i+1) b @ nli i @ "endinstance"
        || mkbdefault ts : " default (" @ mix (map prttype ts) "," @ ")"
	|| mkbsign is t : mix (map oprid is) "," @ " :: " @ prttype t
	end
	where isrec (mkbrec _) = true
	   || isrec _ = false
	and   prbind (ep, mkwhere ges b) = pr1 i ep @ mix (map (\(g,e)." | "@pr1 i g@" = "@pr1 i e) ges) "\n" @ " where " @ prdefg i b
           || prbind (ep, ee) = pr1 i ep @ " = " @ pr1 i ee)
and
    pr1 i e =
	case e in
	   mkmodule id _ imp oexp def :
		nli i @ "module " @ oprid id @
		nli i @ "import " @ (if ImpDebug then mix (map primpid imp) ", " else "...")@";"@
		(case oexp in Some exp : nli i @ "export " @ mix (map prexpid exp) ", " @ ";" || None : "" end) @
		nli i @ prdefg i def @
		nli i @ "end"
	|| mkhmodule id exps imps fixs b :
	        nli i @ "hmodule " @ oprid id @ optpr (\x."(" @ mix (map prexpid x) "," @ ")") exps @
		concmap (\imp.nli i @ primport imp) imps @
		concmap (\fix.nli i @ prfix fix) fixs @
	        nli i @ prdefg i b
	|| mkident ii :
		oprid ii
	|| mkas ii ee :
		oprid ii @ " as " @ pr1 i ee
	|| mkcondp ep ec :
		pr1 i ep @ " & (" @ pr1 i ec @ ")"
        || mklazyp p : 
	        "~(" @ pr1 i p @ ")"
	|| mkinfo t e :
		preinfo t (ppr1 i e)
	|| mkconst c :
		case c in
		   cint ii : itos(ii)
		|| cchar c : ['\'';c;'\'']
		|| cstring s : '"' . s @ "\""
		|| cfloat s : s
		|| cinteger s : s
		|| crational s : s
		end
        || mkbrack gram l:
		 (nli i @ "[|" @ mix (map (prlex i) l) " " @ "|]" @
		 nli i @ if Debug
			 then "in context: " @ nli (i+1) @
			      mix (map (pralts i) gram) (" and" @ nli (i+1))
			 else ""
	         where   pralts i (tt, altlist) =
			  prttype tt @ " ::= " @
			  mix (map ((prprod i) o snd) altlist) " + "
		)
	|| mkap f a :
		(prapchain e
		where rec
		    prapchain (mkap fun arg) =
				if issimple arg then
					prapchain fun @ (' ' . pr1 i arg)
				else
					prapchain fun @ " (" @ pr1 i arg @ ")"
		||  prapchain e = ppr1 i e
		)
	|| mklam ide exp :
		('\\' . pr1 i ide) @ ('.' . pr1 i exp)
	|| mkletv def exp :
		nli i @ "let " @ prdefg i def @
		nli i @ "in " @ pr1 (i+1) exp
	|| mkcase exp casel :
		(nli i @ "case " @ pr1 i exp @ " in" @
		nli (i+1) @ mix (map prcasel casel) (" ||" @ nli (i+1)) @
		nli i @ "end "
		where prcasel (ep, mkwhere ges b) = pr1 (i+1) ep @ mix (map (\(g,e)." | "@pr1 (i+1) g@" : "@pr1 (i+1) e) ges) "\n" @ " where " @ prdefg (i+1) b
                   || prcasel(ep,ee) = pr1 (i+1) ep @ " : " @ pr1 (i+1) ee)
	|| mkerror s : "ERROR " @ s
        || mkcfunction b ii : "("@(if b then "_cFunction" else "_cVariable")@" "@oprid ii@")"
	|| mkconstr c le & (isstring c) : '"'.cname c@"\""
	|| mkconstr c le : (
		 prc (cname c) @
#ifdef DEBUG
		(if Debug then "{" @ itos (cno c) @ ":" @ itos (nconstrs c) @ "}"
		 else "") @
#endif
		 (concmap (\e." "@ppr1 i e) le)
		 where prc ('_'.name) & (~Debug) = name
		 ||    prc x = x)
	|| mkfailmatch n : "DEFAULT_" @ itos n
	|| mklistf L_FROM [e] : "["@ pr1 i e @"..]"
	|| mklistf L_FROM_BY [e1;e2] : "[" @ pr1 i e1 @ ", " @ pr1 i e2 @ "..]"
	|| mklistf L_FROM_TO [e1;e2] : "[" @ pr1 i e1 @ " .. " @ pr1 i e2 @ "]"
	|| mklistf L_FROM_BY_TO [e1;e2;e3] : "[" @ pr1 i e1 @ ", " @ pr1 i e2 @ ".." @ pr1 i e3 @ "]"
	|| mklistg e qs : "["@pr1 i e @" ;; "@mix (map (prq i) qs) "; "@"]"
        || mkwhere ges b : show_list (show_pair (pr1 i, pr1 i)) ges @ " WHERE " @ prdefg i b
	|| mkdo st : nli i @ "do {" @ prstmt (i+1) st @ nli i @ "}"
	|| mkrecord c ies _ : ppr1 i c @ "{" @ mix (map (\ (ii,e) . oprid ii @ "=" @ pr1 i e) ies) ", " @ "}"
	|| _ : fail ("pr. unknown node:\n")
	end
and prstmt i (mksexp e) = nli i @ pr1 i e @ ";"
||  prstmt i (mksexpstmt e s) = nli i @ pr1 i e @ ";" @ prstmt i s
||  prstmt i (mkslet b s) = nli i @ "let " @ prdefg (i+1) b @ ";" @ prstmt i s
||  prstmt i (mksbind p e s) = nli i @ pr1 i p @ " <- " @ pr1 i e @ ";" @ prstmt i s
and prq i (mkqgen p e) = pr1 i p @ " <- " @ pr1 i e
||  prq i (mkqfilter e) = pr1 i e
||  prq i (mkqlet b) = "let "@prdefg i b
end
