module -- misc
--
-- various nice functions
--
#include "../expr/id_t.t"
#include "../expr/constr_t.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/einfo_t.t"
#include "../expr/types_t.t"
#include "../expr/id.t"
#include "../misc/util.t"
#include "hexpr.t"

export leftmost, getids, splitl, prlist, argl, dollar,
	leftmostid, isI, isC, ppart, epart, Useq, gids, countarrows;
rec

    dollar = mkident dummyid
and
    isI (mkident _) = true
 || isI _ = false
and
    isC (mkconstr _ _) = true
 || isC _ = false
and
    ppart (p, _) = p
and
    epart (_, e) = e
and
    leftmost (mkap f a) = leftmost f
 || leftmost (mkcondp p _) = leftmost p
 || leftmost (mklazyp p) = leftmost p
 || leftmost e = e
and
    argl e = (argl1 e []
	     where rec argl1 (mkap f a) l = argl1 f (a.l)
		   || argl1 (mkcondp p _) l = argl1 p l
		   || argl1 e l = l)
-- gids is only used on patterns
and
    lexgids [] = []
 || lexgids ((mklt _).l) = lexgids l
 || lexgids ((mkltint _).l) = lexgids l
 || lexgids ((mkltid _).l) = lexgids l
 || lexgids ((mkltsym _).l) = lexgids l
 || lexgids ((mkunq ex).l) = gids ex @ lexgids l
and
    gids (mkap f a) = gids f @ gids a
 || gids (mklam _ e) = gids e
 || gids (mkcase e pes) = gids e @ concmap (gids o snd) pes
 || gids (mkident i) & (isdummy i) = []
 || gids (mkident i) = [i]
 || gids (mkbrack _ lexl) = lexgids lexl
 || gids (mkconstr _ el) = concmap gids el
 || gids (mkas i e) = i.gids e
 || gids (mkcondp p c) = gids p
 || gids (mklazyp p) = gids p
 || gids (mkinfo t e) = gids e
 || gids (mkrecord _ ies _) = concmap (gids o snd) ies
 || gids _ = []
and
    getids e = mkseteq eqid (gids e)
and
    splitl 0 l = ([],l)
 || splitl n (h.t) = let (a,b)=splitl (n-1) t in (h.a,b)
and
    prlist f l = "[" @ (mix (map f l) "; ") @ "]"
and
    leftmostid e = let (mkident i) = leftmost e in i
and Useq [] u = ([], u)
 || Useq (f.l) u = let (c, u1) = f u  in
		   let (cl, u2) = Useq l u1  in
			(c.cl, u2)

and countarrows t = carrows (synexpandall t)
and carrows (mktcontype k t) = length k + carrows t
||  carrows (mktcons c [_;t]) & (eqid c hiARROW ) = 1 + carrows t
||  carrows _ = 0

end
