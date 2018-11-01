module -- fun
--
-- Turn function definitions (perhaps with pattern matching) into
-- lambda expressions (perhaps with case).
--
#include "../expr/id_t.t"
#include "../expr/constr_t.t"
#include "../expr/ttype_t.t"
#include "../expr/einfo_t.t"
#include "../expr/types_t.t"
#include "../expr/id.t"
#include "../expr/pprint.t"
#include "../transform/misc.t"

export fixfun;
rec
    newids s n = mkident (mkids ('A'.itos n@s))
and fdef funid expr = mkbpat [(funid, expr)]
and pe [] [] = []
||  pe (p.ps) (((mkcondp _ c), e).pes) = ((mkcondp p c), e).pe ps pes
||  pe (p.ps) ((_, e).pes) = (p,e).pe ps pes
-- Try to keep old argument name if possible.
and new fid cp iss = map2 (\is.\n.
#if 0
This code may cause accidental capture of variables.
		       let vs = filter (\v.isI v & (let (mkident i) = v in ~ cp i & ~ isdummy i)) is in
		       if length vs > 0 & allsame vs then
			   hd vs
		       else
			   newid n
#else
			newids (idtostr fid) n
#endif
)
		      iss (from 1)
and mtuple l = revitlist (\a.\f.mkap f a) l (mkident (mkids (tupstr (length l))))
and iswhere (mkwhere _ _) = true
||  iswhere _ = false
and fixfun cp pl =
        let (efunid.erest) = map (leftmost o ppart) pl in
	case efunid in
	    mkident funid :
		if allsame (efunid.erest) then --all ids alike ?
                    let parll = map (argl o ppart) pl in
		    if allsame (map length parll) then
		        let ppl = transpose parll in
			case length (hd parll) in
			   1 :
			    let [nid] = new funid cp ppl in
			    case pe (map hd parll) pl in
				[((p as mkident i), e)] & (nid = p | (isdummy i & ~iswhere e)) : fdef efunid (mklam nid e)	-- fast special case: one argument, no patterns
			    ||  pes : fdef efunid (mklam nid (mkcase nid pes))
			    end
			|| _ :			-- many args, make a tuple
			    let nidl = new funid cp ppl in
			    fdef efunid (itlist mklam nidl (mkcase (mtuple nidl) (pe (map mtuple parll) pl)))
			end
		    else
			mkberror ("[1] Varying number of args to "@oprid funid)
		else
		    mkberror ("[2] Different function names at "@oprid funid)
	|| e : mkberror ("[3] Bad function:"@ppr e)
	end
end
