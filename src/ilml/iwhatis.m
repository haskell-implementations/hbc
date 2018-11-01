module
#include "../misc/flags.t"
#include "../misc/misc.t"
#include "../expr/read.t"
#include "../expr/pprint.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/impexp_t.t"
#include "../expr/impexp.t"
#include "../expr/types_t.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/tinfo.t"
#include "../expr/constrfun.t"
#include "../expr/einfo_t.t"
#include "../ExprE/Eprint.t"
#include "../rename/renenv.t"
#include "imisc.t"
#include "state.t"
export whatis, prenv;

-- Print a Haskell type

rec hpr pi _ (mktvar v) = hprtvar v
||  hpr pi _ (mktcons i []) = pi i
||  hpr pi np (mktap v ts) = paren np (hprtvar v @ " " @mix (map (hpr oprid true) ts) " ")
||  hpr pi np (mktcons i ts) = let l = length ts in
    case idtostr i in
	"P->" & (l=2) : paren np (hpr oprid true (hd ts) @ " -> " @ hpr oprid false (hd (tl ts)))
    ||	"PPrelude.->" & (l=2) : paren np (hpr oprid true (hd ts) @ " -> " @ hpr oprid false (hd (tl ts)))
    ||  "PList" : "["@hpr oprid false (hd ts)@"]"
    ||  "_[]" : "["@hpr oprid false (hd ts)@"]"
    ||  "_Prelude.[]" : "["@hpr oprid false (hd ts)@"]"
    ||  '_'.'#'._ : "("@mix (map (hpr oprid false) ts) ", " @")"
    ||  'P'.'#'._ : "("@mix (map (hpr oprid false) ts) ", " @")"
    ||  'P'.'P'.'r'.'e'.'l'.'u'.'d'.'e'.'.'.'#'.s & (stoi s = l) : "("@mix (map (hpr oprid false) ts) ", " @")"
    ||  _ : paren np (pi i @ " " @mix (map (hpr oprid true) ts) " ")
    end
||  hpr pi _ (mktcontype ts t) = "("@mixmap hpras ts ", " @ ") => " @ hpr pi false t
||  hpr pi _ (mkterror s) = "Error "@s
and hpras (a as mkassert c vs) = oprid c @ " " @ mix (map hprtvar vs) " "
--    hpr false (as2ty a)
||  hpras (mkaerror e) = "ERROR "@e
and hprtvar v =     
    (if v < 0 then "?" else "")@
    let v = if v < 0 then -v-1 else v in
    if v < 25 then [chr (ord 'a' + v)] else 'a'.itos v
and paren true s = "("@s@")"
||  paren false s = s

and dropmodid (i1.i2.is) & (
	let s1 = reverse (tl (idtostr i1)) and s2 = reverse (tl (idtostr i2)) in
	let n1 = length s1 and n2 = length s2 in
	n2 > 0 & n1 > n2+1 & head n2 s1 = s2 & (s1 ?? n2 = '.')
	) 
	= i2 . dropmodid is
||  dropmodid (i.is) = i . dropmodid is
||  dropmodid [] = []
and prenv env = concmap (wprid false) ((if Curry & H1_3 then dropmodid else \x.x) (rids Kvalue env@rids Ktype env))
and hwprid b (i as (mkid _ (c1._) (idi_var _ (Ohastype t _ _) _) _)) & (c1 = '_') =  -- not a default method!!
      hhprid b i@" :: "@hpr oprid false t@"\n"
||  hwprid b (mkid _ _ (idi_type _ t _ ti insts od) _) =
      let fixv vs v = if mem v vs then -v-1 else v in
      let fixt uni vs t = if uni then t else Typerec (mktvar o fixv vs) mktcons mktap t in
      let fixas uni vs cx = if uni then cx else [ mkassert c (map (fixv vs) ts);; (mkassert c ts) <- cx ] in
      let cs = get_cs_from_tinfo ti in
      let dta = if NewType & get_isotype ti then "newtype " else "data " in
      dta@hpr (hhprid b) false t@
      case cs in
	  [] : ""
      ||  _  : " = "@
---mix (map (\(mkcons i (uni,vs,cx) ys _).hprcx (fixas uni vs cx)@hprid i@concmap (\(t,b,_).(if b & H1_3 then " !" else " ")@hpr oprid true (fixt uni vs t)@if b & ~H1_3 then "{-#STRICT#-}" else "") ys) cs) " | "@ if H1_3 then "" else prd od
      mix (map (\ (mkcons i (uni, vs, cx) ys flg).hprcx (fixas uni vs cx)@hprid i@(if flg then " { " else "")@mix (map (\(t,b,sel).prsel flg sel ((if b & H1_3 then " !" else " ")@hpr oprid true (fixt uni vs t)@if b & ~H1_3 then "{-#STRICT#-}" else "")) ys) (if flg then ", " else "") @(if flg then " }" else "")) cs) " | "@ prd od

      end@"\n"
      @hprinsts b (map snd insts)
||  hwprid b (mkid _ _ (idi_class (clsi _ t [] _ insts _)) _) =
      "class "@hprclass b t@"\n"@
      hprinsts b (map snd insts)
||  hwprid b (mkid _ _ (idi_class (clsi _ t dmts _ insts _)) _) =
      "class "@hprclass b t@" where {\n"@mix (map prdmt (filter notpriv dmts)) ";\n"@"\n    }"@"\n"@
      hprinsts b (map snd insts)
||  hwprid b (mkid _ _ (idi_syn _ t1 n t2) _) =
      "type "@hpr (hhprid b) false t1@" = "@hpr oprid false t2@"\n"
||  hwprid _ (i as mkid _ _ (idi_module eis) _) =
      "module "@hprid i@"("@mix (map prexpid eis) ", " @ ")\n"
||  hwprid true (i as mkid _ _ (idi_constr _ _ _ _ _ _ _) _) =
      "constructor "@hhprid true i@" :: "@hpr oprid false (typeof i)@"\n"
||  hwprid true (i as mkid _ _ (idi_method _ _ _) _) =
      "method "@hhprid true i@" :: "@hpr oprid false (typeof i)@"\n"
||  hwprid _ e = ""
and hprcx [] = ""
||  hprcx cx = "("@mixmap hpras cx ", " @ ") => "
and prsel false (Some i) s = "{ " @ hprid i @ " ::" @ s @ "}"
||  prsel true  (Some i) s = hprid i @" ::"@s
||  prsel _ _ s = s
and notpriv (_,i,_) = hd (idtostr i) ~= 'P'
and prdmt (_,m,mktcontype (_.xs) t) = "    "@hprid m @ " :: " @ hpr oprid false (xmkcontext xs t)
and prd None = ""
||  prd (Some []) = ""
||  prd (Some is) = " deriving ("@mix (map hprid is) ", " @ ")"
and hprid i = oprid i
and hhprid true i = 
    case id_orignames i in
       (MI "", _) : oprid i
    || (MI pi, _) & (pi = interactivename) : oprid i
    || (_, "P->") : "Prelude.->"
    || (MI m, old) : 
	if H1_3 then
	    oprid i
	else 
	    tl m @ "." @ oprid i @ (if idtostr i ~= old then " (was "@tl old@")" else "")
    end
||  hhprid false i = oprid i
and hprclass b c = hpr (hhprid b) false (cdecl2type c)
and xmkcontext [] t = t
||  xmkcontext ts t = mktcontype ts t
and hprinsts false _ = ""
||  hprinsts _ [] = "no instances\n"
||  hprinsts _ is = "instances "@mix (map hprinst is) ", " @ "\n"
and hprinst i = hprttype (idecl2type i)

and lwprid _ (i as (mkid _ _ (idi_var _ (Ohastype t _ _) _) _)) =
		oprid i@": "@iprttype t@"\n"
||  lwprid _ (mkid _ _ (idi_type _ t _ ti _ _) _) =
	      let cs = get_cs_from_tinfo ti in
	      "type "@iprttype t@
	          case get_cs_from_tinfo ti in
		      [] : ""
		  ||  cs : " = "@mix (map (\(mkcons i _ ys _).oprid i@concmap (\(t,b,_).' '.iprttype t@if b then "!" else "") ys) cs) " + "
		  end@ "\n"
||  lwprid _ (mkid _ _ (idi_syn _ t1 _ t2) _) =
	      "type "@iprttype t1@" == "@iprttype t2@"\n"
#if 0
||  lwprid _ (mkid _ _ (idi_conctype t g) _) =
	      "conctype "@iprttype t@" = "@mix (map (prprod 10) g) " + " @"\n"
#endif
||  lwprid true (i as mkid _ _ (idi_constr _ _ _ _ _ _ _) _) =
              "constructor "@oprid i@" : "@iprttype (typeof i)@"\n"
||  lwprid _ _ = ""
and iprttype t = iprtt t false
and wprid a i = if Curry then hwprid a i else lwprid a i

and whatis st s = 
    let rec env = st_env st
    and s' = tl s
    and mis = assocdef s' (mapsnd Some (st_menv st)) None
    in case (filter (\i.~eqid i dummyid) [  rfind Kvalue s env
					  ; rfind Ktype s env
					  /*; rfind Kmodule s env*/
					  ],mis) in
	  ([],None) : s' @ " is undefined\n"
       || (is,_) : concmap (wprid true) is @ 
	  case mis in
	     None     : []
	  || Some ids : "module "@s'@"\n" @ indent 2 (prenv ids)
	  end
    end
and indent i = unlines o map (rept i ' ' @) o lines
and unlines = concmap (\x.x@"\n")
and lines "" = []
||  lines s = let (l,s') = splitat '\n' s in l . lines s'
end
