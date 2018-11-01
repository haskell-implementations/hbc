
module -- unify
--
-- unification algorithm
--
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../misc/flags.t"
#include "subst_t.t"
#include "subst.t"
#include "conutil.t"

export Unify, badu;

rec
-- prune shouldn't look inside the substitution, but rather use some function
-- in subst.m!
    prune (mktvar n) (S as ok _ _ s) =
		case assocdef n s (mktvar 0) in
		   mktvar 0 : mktvar n
		|| v : prune v S
		end
 || prune N _ = N

and occursb v (mktvar m) _ = v = m
 || occursb v (mktcontype ts t) s = occursb v t s | exists (occursa v s) ts
 || occursb v (mktcons _ l) s = occursl v l s
 || occursb v (mktap m l) s = v = m | occursl v l s
 || occursb v (mkterror msg) s = fail ("occursb "@msg)
and occursl v [] _ = false
 || occursl v (a.b) s = occursb v (prune a s) s | occursl v b s
and occursa v s (mkassert _ vv) = occursb v (prune (mktvar v) s) s -- ???

and badu s oa ob = 
    let [a; b] = normtypes [oa; ob]
    in  bad [s @ "\n    " @ prttype a @ "\nand " @ prttype b @ "\n"]

and Unify M N = 
    -- take out context first for speed.
    case M in
	mktcontype kM M' :
	    case N in
		mktcontype kN N' : addconTR (combcon kM kN) (unify0 M' N' emptyTR)
	    ||  _ : addconTR kM (unify0 M' N emptyTR)
	    end
    || _ :
	    case N in
		mktcontype kN N' : addconTR kN (unify0 M N' emptyTR)
	    ||  _ : unify0 M N emptyTR
	    end
    end

    where rec
    unify0 Ni Mi s =			-- N0, M0 used in error reporting (this may cost some time!)
        let N0 = prune Ni s
        and M0 = prune Mi s in
	unify1 N0 M0 s
    where rec
    unify1 N M s =
	case N in
	   mktvar n :
		case M in
		   mktvar m : if n = m then s else
				  if n > m then addTR (n, M) s
				           else addTR (m, N) s
		|| mktap m l : addTR (n, M) s
		|| mktcons c l : if occursb n M s then
#if 1
		                        if id_issyn c then
					    unify1 (synexpand M) N s
					else
#endif
					    badu "[63] Cannot unify types (occurence):" (TRtype s N0) (TRtype s M0)
				   else
					addTR (n, M) s
		|| _ : fail ("unify1 "@prttype N@" -- "@prttype M)
		end
	|| mktcons cn ln :
		case M in
		   mktvar m : if occursb m N s then
#if 1
		                  if id_issyn cn then
				      unify1 M (synexpand N) s
				  else
#endif
				      badu "[64] Cannot unify types (occurence):" (TRtype s N0) (TRtype s M0)
			      else
				  addTR (m, N) s
                || mktap vm lm : 
			if id_issyn cn then
			    unify1 (synexpand N) M s
			else
			    unifyap vm lm cn ln s
		|| mktcons cm lm :
			if teqid cn cm then
				unifyl ln lm s
			else
				-- No cause for alarm yet, one (or both) of the ids may be type synonyms.
				-- Check for this and expand synonyms.
				if id_issyn cm then
				    unify1 N (synexpand M) s
				else if id_issyn cn then
				    unify1 (synexpand N) M s
				else
				    badu "[65] Cannot unify types:" N0 M0
		|| _ : fail ("[66] unify1 "@prttype N@" -- "@prttype M)
		end
        || mktap vn ln :
                case M in
		   mktvar m : addTR (m, N) s
		|| mktcons cm lm : 
		       if id_issyn cm then
                           unify1 N (synexpand M) s
		       else
			   unifyap vn ln cm lm s
		|| mktap vm lm : unifyl ln lm (unify1 (mktvar vn) (mktvar vm) s)
		end
	|| _ : fail ("[67] unify1 "@prttype N@" -- "@prttype M)
	end

and unifyl [] [] s = s
 || unifyl (h1.t1) (h2.t2) s = unifyl t1 t2 (unify0 h1 h2 s)
 || unifyl _ _ _ = bad ["[68] unify arity"]

and unifyap i ts1 c ts2 s = 
        let n = length ts2 - length ts1 in
        if n < 0 then
	    badu "[112] h.o. unify" (mktap i ts1) (mktcons c ts2)
	else
	    unifyl ts1 (tail n ts2) (addTR (i, mktcons c (head n ts2)) s)

end
