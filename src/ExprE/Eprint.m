module
--
-- prettyprinter
--
#include "../expr/id_t.t"
#include "../expr/constr_t.t"
#include "../expr/ttype_t.t"
#include "../expr/einfo_t.t"
#include "../expr/types_t.t"
#include "../expr/einfo.t"
#include "../expr/id.t"
#include "../expr/ttype.t"
#include "../expr/constrfun.t"
#include "Expr_t.t"
#include "../transform/misc.t"
#include "../misc/misc.t"
#include "../misc/flags.t"
#include "../expr/impexp.t"

export pr;
rec
    pr e = (if Curry then hpr1 0 e else pr1 0 e) @ "\n"
where rec
    issimple (Evar _) = true
||  issimple (Econstr _ []) = true
||  issimple (Eidapl _ []) = true
||  issimple _ = false
and isinfix (InfixL _) = true
||  isinfix (InfixR _) = true
||  isinfix (Infix _) = true
||  isinfix _ = false
and ppr1 i e = if issimple e then pr1 i e else '(' . pr1 i e @ ")"
and nli i = '\n' . space(4*i)
and prdef i dl = 
	mix (map (\(ii, e). oprid ii @ " = " @ pr1 i e) dl) (nli i @ "and ")
and prc (c as Cconstr _ _ _ _ _ _) & (isstring c) = '"'.cname c@"\""
||  prc (Cconstr ('_'.name) _ _ _ _ _) = pn name
||  prc (Cconstr name _ _ _ _ _) = pn name
and pn (s as c._) = if isalpha c | isdigit c | c = '\'' then s else "("@s@")"
and parpr i arg =
    if issimple arg then
	pr1 i arg
    else
	"(" @ pr1 i arg @ ")"

and isisoalpha c = isalpha c | isiso c
and opr i =
    let s = tl (idtostr i) in
    if Curry & isisoalpha (hd s) then "`"@s@"`" else s
and
    pr1 i e =
	case e in
	   Emodule id exp def :
		"module " @ oprid id @
		"export " @ mix (map prexpid exp) ", " @ ";\n" @
		mix (map (prdef 0) def) "\n{-scc-}\n" @
		"\nend"
	|| Evar ii : oprid ii
	|| Econstr c el : prc c @ (concmap (\e.' '.ppr1 i e) el)
        || Ecfunction b ii : "("@(if b then "_cfunction" else "_cvariable")@" "@oprid ii@")"
        || Eap (Eap (Evar id) a1) a2 & (isinfix (id_fixity id)) :
	        parpr i a1 @ " " @ opr id @ " " @ parpr i a2
	|| Eap f a :
		(prapchain e
		where rec
		    prapchain (Eap fun arg) = prapchain fun @ " " @ parpr i arg
		||  prapchain e = ppr1 i e)
	|| Elam ide exp :
		('\\' . oprid ide) @ ('.' . pr1 i exp)
	|| Elet r def exp :
		nli i @ "let " @ (if r then "rec " else "") @ prdef (i+1) def @
		nli i @ "in " @ nli(i+1) @ pr1 (i+1) exp
	|| Ecase exp casel defe :
		(nli i @ "case " @ pr1 i exp @ " in" @
		nli (i+1) @ mix (map prcasel casel) (" ||" @ nli (i+1)) @
		" ||" @ nli (i+1) @ "_ : " @ pr1 (i+1) defe @
		nli i @ "end "
		where prcasel (c1, idl, ee) = prc c1 @ concmap (\i.' '.oprid i) idl @ " : " @ pr1 (i+1) ee)
	|| Efailmatch n : "DEFAULT_" @ itos n
	|| Eidapl ii l :
		if Debug then
			oprid ii @ "{" @ mix (map (pr1 i) l) "," @ "}"
		else
			oprid ii @ concmap (\e.' '.ppr1 i e) l
	|| Elaml il e :
		if Debug then
			"\\{" @ mix (map oprid il) "," @
			"} " @ pr1 i e
		else
			"(\\" @ mix (map oprid il) " " @
			"." @ pr1 i e @ ")"
	|| Einfo inf e : preinfo inf (pr1 i e)
	|| _ : fail ("pr. unknown node:\n")
	end
and hprdef i (ii, e) = oprid ii @ " = " @ hpr1 i e
and hppr1 i e = if issimple e then hpr1 i e else '(' . hpr1 i e @ ")"
and hparpr i arg =
    if issimple arg then
	hpr1 i arg
    else
	"(" @ hpr1 i arg @ ")"
and hprdefs ds = concmap (\d.hprdef 0 d @ ";\n") ds
and
    hpr1 i e =
	case e in
	   Emodule id exp dss :
	        "module " @ oprid id @ "(" @ mix (map prexpid exp) ", " @ ")" @ " where {\n" @
		mix (map hprdefs dss) "{-scc-}\n" @
		"\n}\n"
	|| Evar ii : oprid ii
        || Ecfunction b ii : "("@(if b then "_cfunction" else "_cvariable")@" "@oprid ii@")"
	|| Econstr c el : 
	        case prc c in
		    'P'.'#'._ : "(" @ mixmap (hppr1 i) el ", " @ ")"
                || s : s @ (concmap (\e.' '.hppr1 i e) el)
		end
        || Eap (Eap (Evar id) a1) a2 & (isinfix (id_fixity id)) :
	        hparpr i a1 @ " " @ opr id @ " " @ hparpr i a2
	|| Eap f a :
		(prapchain e
		where rec
		    prapchain (Eap fun arg) = prapchain fun @ " " @ hparpr i arg
		||  prapchain e = hppr1 i e)
	|| Elam ide exp :
		('\\' . oprid ide) @ (" -> " @ hpr1 i exp)
	|| Elet r def exp :
	        "let "@(if r then "{-rec-} " else "")@"{" @ nli (i+1) @
		mix (map (hprdef (i+1)) def) (";"@nli (i+1)) @
		nli i @ "} in " @ hpr1 i exp 
	|| Ecase exp casel defe :
		(nli i @ "case " @ hpr1 i exp @ " of {" @ nli i @ "  " @
		mix (map prcasel casel) (nli i @ "| ") @
		(if defe = Efailmatch 0 & ~Debug then
		    ""
		else
		    nli i @ "| _ -> "@hpr1 i defe) @
		nli i @ "}" @ nli i
		where prcasel (c1, idl, ee) = prc c1 @ concmap (\i.' '.oprid i) idl @ " -> " @ hpr1 (i+1) ee)
	|| Efailmatch n : "DEFAULT_" @ itos n
	|| Eidapl ii l :
		if Debug then
			oprid ii @ "{" @ mix (map (hpr1 i) l) "," @ "}"
		else
			oprid ii @ concmap (\e.' '.hppr1 i e) l
	|| Elaml il e :
		if Debug then
			"\\{" @ mix (map oprid il) "," @
			"} " @ hpr1 i e
		else
			"(\\" @ mix (map oprid il) " -> \\" @
			" -> " @ hpr1 i e @ ")"
	|| Einfo (restr _ t) e : "("@hpr1 i e@")::"@prttype t
	|| Einfo (trestr t) e : if Debug then "("@hpr1 i e@"):::"@prttype t else hpr1 i e
	|| Einfo (srestr t) e : if Debug then "("@hpr1 i e@")::::"@prttype t else hpr1 i e
	|| Einfo inf e : preinfo inf (hpr1 i e)
	|| _ : fail ("pr. unknown node:\n")
	end
end
