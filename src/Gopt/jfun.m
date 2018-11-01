module -- jfun
#include "../Gcode/Gcodedef_t.t"
#include "../misc/misc.t"
#include "jfunutil.t"
#include "jfun1.t"
#include "jfun2.t"
--
-- A basic block is representad by a triple:
-- entry, exit, pexit, code
-- where entry = entry label (-1 if none, 0 if first block)
--       exit  = exit label (-1 if none)
--       code  = G-code for the block
--
export jfun, blocks;
rec
    joinb nl (e, _, c1) (l, x, c2) =
	(e, x, c1 @ (if nl then [] else [LABEL (Label l)]) @ c2)
and join1 _ _ _ [] = []
||  join1 rs xs used ((b as (e,x,c)).bs) =
	if refcnt e rs = 0 | mem e used then
	    join1 rs xs used bs
	else
	    let jb v y = join1 rs xs (y.used) (joinb v b (extractb y bs) . bs)
	    in
	    if x > 0 then
		if mem x used then
		    (e,x,c@[JMP (Label x)]) . join1 rs xs (e.used) bs
		else
		    jb (refcnt x rs = 1) x
	    else (
	    	case last c in
		    CASE _ _ (Label l) & (~mem l used) : jb false l
		||  CASE _ cl Notalabel & (~mem (ll cl) used) :
			jb false (ll cl)
		||  _ : b.join1 rs xs (e.used) bs
		end
		where ll cl = let (_,_,Label l) = last cl in l)
and join bs =
	let refs = concmap (\(_,_,c).reflist c) bs
	and xrs  = map (\(_,x,_).x) bs
	in join1 (xrs@refs) xrs [] bs
and subl f (JMP l) = JMP (f l)
||  subl f (JFALSE l) = JFALSE (f l)
||  subl f (JTRUE l) = JTRUE (f l)
||  subl f (CASE n ils l) = CASE n (map (\(i,x,l).(i, x, f l)) ils) (f l)
||  subl _ g = g
and remeb f (e,x,c) = (f e, f x, map (subl g) c
		where g (Label l)=Label (f l)
		   || g n=n)
and remempty bs =
	let (es, nes) = partition empty bs in
	let tr = map (\(e,x,_).(e,x)) es in
	-- not very efficient!!
	let rec f l = let l1 = assocdef l tr l in
		      if l = l1 then l1 else f l1 in
	map (remeb f) nes
and blocks = join o remempty o remdup o map bopt o choplist bblock
and jfun = concmap addlabel o blocks
end
