module -- prexport
-- Print the export list of a module.
-- The main program only gets its type printed.
-- Only ordinary identifiers (beginning with '_') gets printed, not
-- stuff produced with the -Z flag, nor constructor functions.
#include "Expr_t.t"
#include "../expr/types_t.t"
#include "../expr/ttype_t.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/ttype.t"
#include "../expr/einfo.t"
#include "../expr/tinfo.t"
#include "../expr/pprint.t"
#include "../misc/flags.t"
#include "../expr/impexp_t.t"
#include "../expr/impexp.t"
export prexport;
rec prexport (Emodule i [mkexpid (mkid _ "Pmain" (idi_var _ (Ohastype t _ _) _) _)] _) =
	lprttype t @ "\n"
||  prexport (Emodule i exps _) =
                let is = map expid exps in
                concmap prfix (concmap ecollid is) @
		(concmap p is
		where p (i as (mkid _ _ (idi_var (var_global f) (Ohastype t _ _) _) _)) =
		    if Fullname | hd (idtostr i) = '_' then
			let si = oprid i in "import "@si@": "@lprttype t@prfinfo f si@";\n"
		    else
			""
		  || p (i as (mkid _ _ (idi_var _ (Ohastype t _ _) _) _)) =
		    if Fullname | hd (idtostr i) = '_' then
			"import "@oprid i@": "@lprttype t@";\n"
		    else
			""
		  ||  p (mkid _ _ (idi_type _ t _ ti _ _) _) =
		      let cs = get_cs_from_tinfo ti in
		      "import type "@lprttype t@" = "@mix (map (\(mkcons i _ ys _).oprid i@concmap (\(t,b,_).' '.plprttype t@if b then "!" else "") ys) cs) " + " @ ";\n" --XXX
                  ||  p (mkid _ _ (idi_syn _ t1 _ t2) _) =
		      "import type "@lprttype t1@" == "@lprttype t2@";\n"
		  ||  p (mkid _ _ (idi_conctype t g) _) =
		      "import conctype "@lprttype t@" = "@mix (map (prprod 10) g) " + " @";\n"
		  ||  p _ = "")

and plprttype t = lprttype t
and ecollid (i as (mkid _ _ (idi_var _ _ _) _)) = [i]
||  ecollid (i as mkid _ _ (idi_type _ _ _ ti _ _) _) = i . map (\(mkcons i _ _ _).i) (get_cs_from_tinfo ti)
||  ecollid _ = []
and prfix i =
    case id_fixity i in
	Infix  n   : "infixn  \""@pid i@"\";\n"
    ||  InfixL n   : "infix   \""@pid i@"\";\n"
    ||  InfixR n   : "infixr  \""@pid i@"\";\n"
    ||  FPrefix n  : "prefix  \""@pid i@"\";\n"
    ||  FPostfix n : "postfix \""@pid i@"\";\n"
    ||  Nonfix     : "nonfix  \""@pid i@"\";\n"
    ||  Nofixity : ""
    end
and pid i = concmap prc (tl (idtostr i))
and prc '\\' = "\\\\"
||  prc c = [c]
end
