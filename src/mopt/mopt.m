module -- mopt
--
-- Take care of some common cases in the mcode, which are not fixed in
-- the mcode generator.
--
#include "../mcode/mcodedef_t.t"
#include "../mcode/mutil1.t"
#include "../mcode/mprint.t"
#define CASEFILL 5
#define MAXHIGH 100
export mopt;
rec
    mnocom (Mcom _.M) = mnocom M
||  mnocom (m.M) = m.mnocom M
||  mnocom [] = []
and adj n (Sind m) = Sind (m-n)
||  adj n (Srel m) = Srel (m-n)
||  adj n a = a
and hadj n (hpind m) = hpind (m-n)
||  hadj n (hprel m) = hprel (m-n)
||  hadj n a = a
and hasSp pushS = true
||  hasSp popS = true
||  hasSp (Sind _) = true
||  hasSp _ = false
and
    -- remove redundant load from stack
    mpeep ((m as Mmove (reg r1) pushS).Mmove (Sind 0) (reg r2).M) & (r1 = r2) =
	mpeep (m.M)
||  mpeep ((m as Mmove (reg r1) pushS).Mmove (Sind 0) a.M) =
	mpeep (m.Mmove (reg r1) a.M)
    -- improve address calculations
||  mpeep (Mmove (hprel 0) hp.M) = mpeep M
||  mpeep (Mmove (hprel 0) a.M) = Mmove hp a.mpeep M
||  mpeep (Mmove (Vrel 0) Vp.M) = mpeep M
||  mpeep (Mmove (Vrel 0) a.M) = Mmove Vp a.mpeep M
||  mpeep (Mmove (Srel 0) Sp.M) = mpeep M
||  mpeep (Mmove (Srel 0) a.M) = Mmove Sp a.mpeep M
||  mpeep (Mmove (regrel r 0) (reg r1).M) & (r = r1) = mpeep M
||  mpeep (Mmove (regrel r 0) a2.M) = Mmove (reg r) a2.mpeep M
    -- remove redundant jump
||  mpeep (Mjump l1.M as (Mlabel l2._)) & (l1 = l2) = mpeep M
    -- remove jump over jump that Gopt handles badly
||  mpeep (Mjcond cc l1.Mjump l2.M as (Mlabel l3._)) & (l1=l3) =
	Mjcond (negmop cc) l2.mpeep M
    -- improve returning a variable
||  mpeep (Mmove a (Sind n1).
	   Mmove (Srel n2) Sp.Mmove popS (reg r).M) & (n1=n2) =
	mpeep (Mmove a (reg r).Mmove (Srel(n2+1)) Sp.M)
    -- use autoincrement
||  mpeep (Mmove (Sind 0) a.Mmove (Srel 1) Sp.M) & (a~=pushS) =
	Mmove popS (adj 1 a).mpeep M
    -- improve ALLOC
||  mpeep ((m1 as Mmove (idlit _) tohp).
	   (m2 as Mmove (hprel 2) hp).
	   Mmove (hprel(-3)) a.M) & (a ~= tohp) =
		Mmove hp (hadj (-3) a).m1.m2.mpeep M
    -- rearrange moves to get better addressing modes
||  mpeep (Mmove a pushS.Mmove (Sind 0) (reg r).M) =
	Mmove a (reg r).Mmove (reg r) pushS.mpeep M
    -- remove redundant push 
||  mpeep (Mmove a pushS.M as (Mmove popV Sp._)) =
	mpeep M
    -- clean up silly popping
||  mpeep (Mmove popS (Sind 0).Mmove popS a.ms) & (~hasSp a) =
        Mmove (Sind 0) a.Mmove (Srel 2) Sp.mpeep ms
    -- fill out case so the limit check can be avoided
||  mpeep (Mcase a l h m ls x.ms) & (l >= 0 & h < m & m < MAXHIGH & m - (h-l+1) > 0 & m - (h-l+1) <= CASEFILL ) =
        let lbl = "LCF"@itos x in
        Mcase a 0 (m-1) m (rept l lbl @ ls @ rept (m-h-1) lbl) x . Mlabel lbl . mpeep ms
    -- combine gettag & eval
||  mpeep (Mcalltag oeval _ . Mmove (regind 0 0) _ . (ms as Mcalltag ogettag _ . _)) =
        mpeep ms
||  mpeep (Mcalltag oeval _ . (m1 as Mmove (reg 0) pushS) . Mmove (Sind 0) (reg 0) . Mmove (regind 0 0) _ . (m2 as Mcalltag ogettag _) . ms) =
        m2.m1.mpeep ms
    -- all other cases
||  mpeep (m.M) = m.mpeep M
||  mpeep [] = []

#if 0
-- temporary check for memory checks
and mcheck ms =
    if badmem ms then fail "Bad memory check" else ms
and badmem ms = exists badfun (choplist makefun (deldata true ms))
and makefun [] = ([], [])
||  makefun (Mfunbegin _ _.ms) = splitat Mfunend ms
||  makefun (m.ms) = makefun ms
and badfun ms = exists badblock (choplist makeblock (remgarb ms))
and remgarb (_._.Mcall "GARB"._.ms) = Mcom "GC".remgarb ms
||  remgarb (_.Mcall "NGARB".ms) = Mcom "GC".remgarb ms
||  remgarb (m.ms) = m.remgarb ms
||  remgarb [] = []
and makeblock [] = ([],[])
||  makeblock ((m as Mcall _).ms) = ([m],ms)
||  makeblock ((m as Mjump _).ms) = ([m],ms)
||  makeblock ((m as Mjumpind _).ms) = ([m],ms)
||  makeblock ((m as Mcallind _).ms) = ([m],ms)
||  makeblock ((m as Mjumpf _).ms) = ([m],ms)
||  makeblock ((m as Mreturn).ms) = ([m],ms)
||  makeblock ((m as Mcalltag _ _).ms) = ([m],ms)
||  makeblock ((m as Mjumptag _ _).ms) = ([m],ms)
||  makeblock ((m as Mjcond _ _).ms) = ([m],ms)
||  makeblock ((m as Mlabel _).ms) = ([m],ms)
||  makeblock ((m as Mcase _ _ _ _ _ _).ms) = ([m],ms)
||  makeblock (m.ms) = let (x,y) = makeblock ms in (m.x, y)
and badblock ms = if allocbeforegc ms then trace (mprint ms) true else false
and allocbeforegc [] = false
||  allocbeforegc (Mcom "GC"._) = false
||  allocbeforegc (Mmove _ tohp._) = true
||  allocbeforegc (Mmove _ hp._) = true
||  allocbeforegc (_.ms) = allocbeforegc ms
and deldata _ (Mdata.ms) = deldata false ms
||  deldata _ (Mtext.ms) = deldata true ms
||  deldata true (m.ms) = m.deldata true ms
||  deldata false (m.ms) = deldata false ms
||  deldata _ [] = []
#endif
and
    mopt = /*mcheck o*/ mpeep o mnocom
end
