module -- einfo
#include "../misc/flags.t"
#include "id.t"
#include "einfo_t.t"
#include "ttype.t"
#include "ttype_t.t"
#include "pprint.t"
#include "booltree.t"
export preinfo, prfinfo, framesize_of_finfo, arity_of_finfo, hprfinfo, entries_of_finfo, f_unk, pr1entry;
rec preinfo strict s = "("@s@"){STRICT}"
||  preinfo inline s = "("@s@"){INLINE}"
||  preinfo noeval s = s@"`"
||  preinfo chkind s = s@"``"
||  preinfo (restr _ t) s = "("@s@":"@prttype t@")"
||  preinfo (spark is) s = "("@s@") {SPARK "@mix (map idtostr is) " " @ "}"
||  preinfo (doeval is) s = "("@s@") {EVAL "@mix (map idtostr is) " " @ "}"
||  preinfo (trestr t) s = "("@s@"::"@prttype t@")"
||  preinfo (srestr t) s = if Debug then "("@s@":::"@prttype t@")" else s
||  preinfo (noarrow t) s = "("@s@"{NOARROW "@prttype t@"})"
||  preinfo notchk s = "(("@s@"){NOTCHK})"
||  preinfo (forcearity n) s = "("@s@"{FARITY "@itos n@"})"
||  preinfo overload s = "(("@s@"){OVERLOAD})"
--||  preinfo (metsel n a) s = "(("@s@"){METSEL "@itos n@","@itos a@"})"
--||  preinfo (vecsel) s = "(("@s@"){VECSEL})"
||  preinfo (metcall) s = "(("@s@"){METCALL})"
||  preinfo (vecreg2) s = "(("@s@"){VECREG2})"
||  preinfo (vectordef) s = "(("@s@"){VECTORDEF})"
||  preinfo (limitok) s = "(("@s@"){LIMITOK})"
||  preinfo unboxedvar s = "(("@s@"){UNBOXEDVAR})"
||  preinfo unboxedarg s = "(("@s@"){UNBOXEDARG})"
||  preinfo unboxedexpr s = "(("@s@"){UNBOXEDEXPR})"
||  preinfo unboxvars s = "(("@s@"){UNBOXVARS})"
||  preinfo (specialcall ips) s = "(("@s@"){SPECIALCALL "@map pr1entry ips@"})"
||  preinfo notconst s = "(("@s@"){NOTCONST})"
||  preinfo stmtpat s = "(("@s@"){STMTPAT})"
||  preinfo (dropctx t vs) s = "(("@s@"){DROPCTX "@prttype t@";"@mix (map (prttype o mktvar) vs) ","@"})"
||  preinfo (position f l) s = "(("@s@"){POSITION "@f@" "@itos l@"})"
||  preinfo (stmtfailpoint _) s = "(("@s@"){STMTFAIL})"
and
    prfinfo f i = prf "{" "}" f i true
and 
    hprfinfo b i f = prf "{-" "-}" f i b
and
    prf b e (finfo a es (s,t) fs il) i xx = 
  let ai = AnnotId | ~xx in
    " "@
    (if a ~= -1 & xx then
	 b@prr ai i "ARITY" (itos a)@e
     else
	 "")@
    (if a ~= -1 then
	 b@prr ai i "STRICTNESS" ("\""@xxprs s@","@prs t@"\" ST")@e
     else
	 "")@
    case es in
	[] : ""
    ||  _ : b@prr ai i "ENTRY" ("\"" @ mix (map (map pr1entry) es) "\" \"" @ "\"")@e
    end @
    case il in
        None : ""
    ||  Some exp : b@prr ai i "INLINE" (ppr exp)@e
    end@
    if nuflag then
	b@prr ai i "FRAMESIZE" (itos fs)@e
    else
	""
and
    prr ai id p s = "# "@p@" "@(if ai then id else "_")@" = "@s@" #"
and xxprs x = if x = btff then "T" else prs x
and prs x & (x = bttt) = "T"
||  prs x & (x = btff) = "F"
||  prs (btands xs) = mix (map prsor xs) "&"
and prsor (btors [btvar x]) = itos x
||  prsor (btors xs) = "(" @ mix (map (\(btvar v).itos v) xs) "|" @ ")"
||  prsor bt = fail ("prsor: "@show_BT bt)
and pr1entry (AInothing,APframe) = 'N'
||  pr1entry (AIeval,   APframe) = 'E'
||  pr1entry (AIunboxed,APframe) = 'U'
||  pr1entry (AInothing,APregister) = 'n'
||  pr1entry (AIeval,   APregister) = 'e'
||  pr1entry (AIunboxed,APregister) = 'u'

and framesize_of_finfo (finfo _ _ _ n _) = n
and entries_of_finfo (finfo _ ss _ _ _) = ss
and arity_of_finfo (finfo n _ _ _ _) = n
and f_unk = finfo (-1) [] (bttt, btff) (-1) None
end
