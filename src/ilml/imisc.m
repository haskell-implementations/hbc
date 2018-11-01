module
#include "../misc/flags.t"
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
#include "../expr/einfo_t.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Eprint.t"
#include "../rename/renenv.t"
#include "../rename/renameutil.t" /* mkcompound */
#include "../rename/buildinsts.t"
#include "../rename/buildclass.t"
#include "../transform/remsign.t"
#include "../transform/hexpr.t"
#include "cast.t"
#include "cexpr.t"
#include "icomp.t"
#include "iabs.t"
#include "idsubst.t"
#include "compile.t"
#include "state.t"
export cexpr, cbind, cmodule, mkmodb, interactivename, typeof, iprtt, ibuildinsts, ibuildsuper, ifail, mappair, number,ReallyLoadShareLib;
rec ii = mkident iid
and iid = mkids interactivename
and interactivename = "Pinteractive"
and mka = mkap o mki
and mki = mkident o mkids
and mkmode e = mkmodb (mkbpat [(ii, e)])
and mkmodb b =
    if Curry then
	mkhmodule iid None [] [] b
    else
	mkmodule (mkids "_") [] [] (Some []) b
and cexpr st e0 =
    let rec
	cmple e c = case icomp (st_def st) (st_env st) (mkmode e) true true (st_u st) in
		       Yes ([(i,e)], env, _, _, _) : c e (typeof (rfind Kvalue (idtostr i) env))
		    || No msg : No msg
		    end
    and cmple' e = cmple (if H1_3 then mka "__ioToDialogue" e else e) (\e.\t.Yes e)
    in cmple e0 (\e.\t.
       if Curry then
	   case t in -- should really try unification here
	      mktcons vfun [mktcons vlst1 [mktcons vresp []];mktcons vlst2 [mktcons vreqs []]] & 
              (~ H1_3 & eqid vfun hiARROW & eqid vlst1 hiList & eqid vresp hiResponse & eqid vlst2 hiList  & eqid vreqs hiRequest) : Yes e
	   || mktcons vdia [] & (~ H1_3 & eqid vdia hiDialogue) : Yes e 
	   || mktcons vio [ta] & ( H1_3 & eqid vio hiIO) : 
	      case ta in 
		 mktcons vunit [] & (eqid vunit hiUnit) : cmple' e0
	      || _ : cmple' (mkap (mka "_>>=" e0) (mki "_print"))
	      end
	   || _ : cmple' (mka "_print" e0)
	   end
       else
	  let showf = makeshow (st_env st) t in
	  case icomp (st_def st) (st_env st) (mkmode (mkident (mkids "_printString"))) true true (st_u st) in
	     Yes ([(_, prStr)], _, _, _, _) :
	     if stringp t then
		 Yes (Ap prStr e)
	     else
		 Yes (Ap prStr (Ap showf e))
	  || No msg : No msg
	  end)
and typeof i =
    case type_of_id i in
/*	Ohastype (mktcontype k t) _ _ : let vs = getTvars t in case filter (\ (mkassert c v).mem v vs) k in [] : t || k' : mktcontype k' t end
    ||*/	Ohastype t _ _ : t
    ||  _ : ifail ("No type found for: "@prid i)
    end
and extl lib bs =
	let rec lib' = new @ lib
	and new = map (\(i,e).Internal i ((Comb (prid i) o compile o idsubst lib') e)) bs
 	in lib'
and cbind st b =
    let env = st_env st in
	case icomp (st_def st) env (mkmodb b) true false (st_u st) in
	   Yes (ies, env', env'', u, d) : Yes (env', st_mk env'' (extl (st_lib st) ies) u d (st_menv st) (st_lds st), ies)
	|| No msg : No msg
	end

and cmodule st e =
    let env = st_env st in
	case icomp def_dflts env e false true (st_u st) in
	   Yes (ies, env', env'', u, _) : Yes (env', st_mk env'' (extl (st_lib st) ies) u (st_def st) (st_menv st) (st_lds st))
	|| No msg : No msg
	end

and typeok (mktvar _) = true -- ??
||  typeok (mktcons i ts) = idtostr i ~= "P->" & all typeok ts
||  typeok (mktap _ _) = false
and iprtt (mktvar v) _ = '*'.if v < 10 then [chr(ord 'a' + v)] else 't'.itos v
 || iprtt (mktap v lt) p = "(" @ ('*'.if v < 10 then [chr(ord 'a' + v)] else 't'.itos v) @ concmap (\x." "@iprtt x true) lt @ ")"
 || iprtt (mktcons i []) _ = oprid i
 || iprtt (mktcons i lt) p = (
	case idtostr i in
	    "P->" : cpr p "("@iprtt(hd lt)true@"->"@iprtt(hd (tl lt))false@cpr p ")"
	||  "_#2" : cpr p "("@iprtt(hd lt)true@"#"@iprtt(hd (tl lt))true@cpr p ")"
	||  _ : cpr p "(" @ oprid i @ concmap (\x." "@iprtt x true) lt @ cpr p ")"
	end
	where cpr true s = s
	||    cpr false _ = "")

and Vseq = Comb "seq" (cast (\x.\y.seq y x))
and showK t = Ap Vseq (Constant (CString ("<<"@iprtt t false@">>")))
and stringp (mktcons l [mktcons c []]) = eqid l hiList & eqid c hiChar
||  stringp (mktcons l []) = eqid l hiString
||  stringp _ = false
and makeshow r (t as mktvar _) = showK t
||  makeshow r (t as mktcons i ts) = 
	if stringp t then
		Var (rfind Kvalue ("_show_String") r)
	else
		let i = rfind Kvalue ("_show_"@prid i) r in
		if i ~= dummyid then
			reduce (\x.\y.Ap y x) (Var i) (rev (map (makeshow r) ts))
		else
			showK t

and ibuildinsts css menv env =
    if null (rids Ktype menv) then
	(rgetct env, [])
    else
	buildinsts css env
and ibuildsuper menv env =
    if null (rids Ktype menv) then
	case rgetct env in
	   (_,_,s) : (s, [])
        end
    else
	buildsuper env

and ifail msg = fail ("Internal error: "@msg)

and mappair f g (a,b) = (f a,g b)

and number i [] = []
||  number i (x.xs) = (i,x).number (i+1) xs 

and ReallyLoadShareLib = 
    LoadShareLib
#ifndef I_DYNLIB
    & false
#endif
#ifndef I_AOUT
    | true
#endif

end
