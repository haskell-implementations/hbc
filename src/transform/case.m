module
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/types_t.t"
#include "../expr/constr_t.t"
#include "../expr/subst.t"
#include "../expr/error.t"
#include "../expr/pprint.t"
#include "../expr/ttype.t"
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../misc/sort.t"
#include "misc.t"
#include "failcase.t"
#include "caseutil.t"
#include "casep.t"
#include "exprs.t"

export caseelim;
rec
    -- Complex pattern elimination
    -- Second parameter is always a list of identifiers.
    g d [] es u = (condjoin d es, u)
 || g d (i.il) [pe as (p0._, _)] u & (isconstp p0) =
        let (p.pl, e) = substas i pe in
	let (e1, u) = g d il [(pl, e)] u in (cmpconst i p e1 d, u)
 || g d (i.il) opl u = 
    if (typeerror o filter isC o map (rmas o hd o fst)) opl then
	((merror (mkcase i (mapfst hd opl)) "[71] Mismatching constructor type"), u)
    else
        (revitlist g1 (splitup (map (substas i) opl)) (d, u)
	where g1 pl (d, u) = 
 	(
	let (dl, npl) = partition ispI pl in
	let pll = groupsort gtCs npl in
	    Uap (fmkcase i) (Useq (hI dl . map hC pll) u)
    where
        hI [] u =
		(mfail d, u)
     || hI [(p.pp, e)] u =
		Uap (\e1.(p, e1)) (g d il [(pp, e)] u)
     || hI pl u =
		let I = newid u in
		Uap (\e1.(I, e1)) (g d il (subfstp I pl) (u+1))
    and hC (pl as (((mkconstr C el)._, _)._)) u =
	    -- The constructor arity has been checked in constr.m
	    let nel = length el in
	    let ess = transpose (map (\((mkconstr _ es)._,_).es) pl) in
	    let ids = map2 keepid (from u) ess in
	    Uap (\e1.((mkconstr C ids), e1))
		(g (mkfailmatch 1) (ids@il) (map (\ ((mkconstr _ el).p, e).(el@p, e)) pl) (u+nel))
))

and okid (mkident i) = ~isdummy i
||  okid _ = false
and eistr (mkident i) = idtostr i
-- Try to make a name that looks as the old ones if feasible.
and keepid k es =	-- Much faster is just "newid k", but it looks worse in error messages
    let eis = filter okid es in
    mkident(
    if length eis > 0 & allsame (map eistr eis) then
        case hd eis in
        mkident (mkid _ s f on) : mkid k s f on
        end
    else
	mknewid "I" k)

and caseelim fn (ce as mkcase e ps1) u =
    let ps = mapfst rmstring ps1 in
    let pps = map (rmas o fst) ps in
    if overlap pps then
        (merror ce "[70] Later pattern completely overlapped", u)
--    else if (typeerror o filter isC o map dropcond) pps then
--        (merror ce "[71] Mismatching constructor type", u)
    else case e in
	 -- Handle single identifiers for efficiency reasons.
            mkident _ :
	     g (tfail ("No match in "@fn)) [e] (reverse (map fixp ps)) u

	 -- Handle n-tuples that arises from n-ary function definitions.
	 -- It is not necessary to do it here, but it saves work later on
	 -- in the compiler.
	 || mkconstr (c as Cconstr ('P'.'#'._) _ _ _ _ _) es &
	    (all isI es &
	     all p pps
		where p (mkconstr c1 _) /*& (c = c1)*/ = true
                ||    p (mkcondp (mkconstr c1 _) _) /*& (c = c1)*/ = true
		||    p _ = false) :
	     g (tfail ("No match in "@fn)) es (reverse (map ((\([mkconstr _ ps],e).(ps,e)) o fixp) ps)) u

	 || _ :
             let (i as mkident id) = newid u in
	     let (d, u) = g (tfail ("No match in "@fn)) [i] (reverse (map fixp ps)) (u+1)
	     in
	     -- Avoid creating a let expression if there is only one occurence (not inside lambda) of i
             case countocc id d in
		 0 : (d, u)
	     ||  1 : (subst e id d, u)
             ||  _ : (mkletv (mkbpat [(i, e)]) d, u)
	     end
	 end
end
