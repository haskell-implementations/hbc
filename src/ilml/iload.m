module
#include "cexpr.t"
#include "../expr/ttype.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/types_t.t"
#include "../expr/impexp_t.t"
#include "../rename/import.t"
#include "../rename/renenv.t"
#include "../rename/buildinsts.t"
#include "../rename/deriv.t"
#include "../rename/renameutil.t"
#include "../misc/flags.t"
#include "imisc.t"
#include "loadmodule.t"
#include "dynlib.t"
#include "state.t"

infixr ">>";

export cload, dconvies, ccload,dloadmodule,dloadmodules,baseName,
    stripextension;

rec cload st ss imps =
	case dloadmodules ss in
	   No msg : No msg
	|| Yes ies : 
               let s = hd ss
	       and oenvi = st_env st
	       and imp = mkimport (mkids "_lmlimports") [] [] imps false [] [] false None in
	       let rec (u, pienv, _) = importe (chkfun oenvi) s (st_u st) renvi [] [imp]
	       and     ienv = runperm pienv
	       and     renvi = rjoin ienv oenvi in
	       let lib' = dconvies ienv ies in
	       Yes (ienv, st_mk renvi (lib' @ st_lib st) u (st_def st) (st_menv st) (st_lds st))
	end

and runperm r = rjoin (rjoin (rjoin (rlist Kvalue (rids Kvalue r)) (rlist Ktype (rids Ktype r))) (rlist Kmodule (rids Kmodule r))) (rlist Kmeth (rids Kmeth r)) -- !!! runperm or fix rjoin
and gsupers env = let (_, _, sup) = rgetct env in sup

and ccload st ss imp =
    let s = hd ss in
    let oenvi = st_env st in
    let rec (u1, pienv, emsg) = importe (chkfun oenvi) s (st_u st) renvi [] [imp]
    and     ienvi = runperm (addmets pienv)		-- imported environment
    and     dienvi = rjoin denvi ienvi              -- imported & derived environment
    and     nenvi = rjoin dienvi oenvi              -- new environment, except supers etc.
    and     (css, circs) = ibuildsuper ienvi nenvi
    and     (eder, denvi, _, u) = solvederiv AutoDerive (ifail "ff-ccload") rnil ienvi oenvi u1
    and     (cts, _) = ibuildinsts css ienvi nenvi
    and     renvi = rsetct nenvi cts
    in
	if ~null emsg then
	    No emsg
	else if ~null eder then
	    No ("[7] Bad deriving for: "@mix eder ", ")
	else if circs ~= [] then
	    No ("[9] Circular class structure "@mix circs ", ")
	else
(if X4 then trace (show_Renv dienvi) else (\x.x))
(
            case  dloadmodules ss in
	       No msg  : No msg
	    || Yes ies : Yes (ienvi, st_mk renvi (dconvies dienvi ies @ st_lib st) u (st_def st) (st_menv st) (st_lds st))
	    end
)

and chkfun env s = eqid (rfind Ktype s env) dummyid

and convies r ies = concmap (conv r) ies
and conv r (Internal s0 e) =
    let rec (_, s) = idasm s0    -- !!! module name is ignored
    and try m notf = 
	case rfind m s r in
	   (i as mkid k s _ _) & (k ~= 0) : [ Internal i (Comb s e) ]
	||  _ : notf
	end
    in try Kvalue (if Curry then try Kmeth [] else [])

and dconvies :: Renv -> [Symbol String Univ] -> [Symbol Id Cexpr]
and dconvies renv ies = 
    if ReallyLoadShareLib then map (\(DynLib s h).DynLib s h) ies 
    else convies renv ies

and idasm s = idasm' "" "" s
and idasm' m i "" = (reverse m, reverse i)
||  idasm' m i ('$'.'_'.cs) = idasm' i "" cs
||  idasm' m i ('$'.c1.c2.cs) = idasm' m (chr (hex c1 * 16 + hex c2) . i) cs
||  idasm' m i (c.cs) = idasm' m (c.i) cs
and hex c = if c <= '9' then ord c - ord '0' else ord c - ord 'A' + 10

and dloadmodule :: String -> OK String [Symbol String Univ]
and dloadmodule s = dlm (dlo [s]) (loadmodule s)

and dloadmodules :: [String] -> OK String [Symbol String Univ]
and dloadmodules ss = dlm (dlo ss) (loadmodules ss)

and dlm dloader aloader = 
    if ReallyLoadShareLib 
    then dloader
    else aloader >> (Yes o map (\(s,u).Internal s u))

and dlo [] = Yes []
||  dlo (s.ss) = dlopen s >> (\h.
		 dlo ss   >> (\hs.
		 Yes (DynLib s h . hs)))

and baseName = strippath o stripextension
and strippath s = rev (fst (splitat '/' (rev s)))
and stripextension s = let np = rev (snd (splitat '.' (rev s)))
		  in if null np then s else np

and (>>) :: OK *e *a -> (*a -> OK *e *b) -> OK *e *b
and m >> f = case m in
		No msg : No msg
	     || Yes a  : f a
	     end
end
