module -- mvectable
#include "mcodedef_t.t"
#include "../misc/flags.t"
#include "../main/files.t"
#include "magic.h"
export vectable, adds, addv, snum, v2l, addthem, addthemb;
rec
    vectable (strs, vecs, _) = 
	let nstrs = adds [groupname; modulename] strs in
	let snumg = snum groupname nstrs in
	let snumm = snum modulename nstrs in
	if ProfileHeap then
	    vecs2code snumg snumm vecs @ strs2code nstrs 
	else
	    []


and vecs2code sg sm [] = []
||  vecs2code sg sm ((v as (sp,sc,st)) . vs) = 
          Malign		.
          Mlabel (v2l v)	. 
          Mword (const HEAPMAGIC).      -- magic number to check consistency
          Mword (glob (s2l sg))	.	-- group
          Mword (glob (s2l sm)) .	-- module
          Mword (glob (s2l sp)) .	-- producer
          Mword (glob (s2l sc)) .	-- construction
          Mword (glob (s2l st)) .	-- type
	  vecs2code sg sm vs

and addthem (strs, vecs, fstr) (cstr, tstr) =
        let nstrs = adds [fstr;cstr;tstr] strs in
        let vec   = (snum fstr nstrs, snum cstr nstrs, snum tstr nstrs) in
        let nvecs = addv vec vecs in
        ((nstrs, nvecs, fstr), vec)

and addthemb (strs, vecs, fstr) ccs =
        let nstrs = adds [fstr;ccs] strs                       	    in
        let snumc = snum ccs nstrs                                  in
        let vec   = (snum fstr nstrs, snumc, snumc)                 in
        let nvecs = addv vec vecs                              	    in
	((nstrs, nvecs, fstr), vec)

and strs2code = s2c 0

and s2c i [] = []
||  s2c i (s.ss) = Mlabel (s2l i) . Mstring (drop_ s) . s2c (i+1) ss
and drop_ ('_'.cs) = cs
||  drop_ cs = cs
/*
 *	Add some new strings to the old ones that we already have.
 *	We must preserve the ordering of the list during this operation
 *	because snum uses the position within the list to assign a 
 *	unique number to each string.
 */

and adds nss oss = reduce ads oss (mkset nss)

and ads s ss & (mem s ss) = ss		-- hopefully, this happens a lot
||  ads s ss = ss @ [s]			-- and this not too much 

/*
 *	Add a new vector to the old ones that we already have.
 *	There's no need to preserve list ordering here, so we
 *	keep things ordered for efficiency.
 */

and addv = insert 

and insert e [] = [e]
||  insert e (es as (x.xs)) & (e < x) = e . es
||  insert e (es as (x.xs)) & (e = x) = es
||  insert e (es as (x.xs)) & (e > x) = x . insert e xs

/*
 * 	Return the string number (position) string s in the
 *	list of strings ss. This operation should not fail.
 */

and snum = snum1 0

and snum1 p s [] = fail ("snum: can't find " @ s)
||  snum1 p s (x.xs) & (s = x) = p
||  snum1 p s (x.xs) = snum1 (p+1) s xs 

and v2l (pn,cn,tn) = 'L'.'V'. itos pn @ '_' . itos cn @ '_' . itos tn

and s2l i = 'L' . 'S' . itos i 

end
