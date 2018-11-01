module
#include "../mcode/mcodedef_t.t"
#include "tmp.h"

export mtrans3;

rec align n = ((n+3)/4)*4

and splitconst 0 s = []            -- splitconst works only on positive numbers !
 || splitconst n s = if n%4 = 0 then splitconst (n/4) (s+2) 
                                else (n%256,s).splitconst (n/256) (s+8)
and aconst 0 = 1
 || aconst (-1) = 1
 || aconst n & (n>0) =
       let cl = splitconst n 0
       in length cl
 || aconst n & (n<0) =
       let cl = splitconst ((-n)-1) 0           -- neg = inc o not
       in length cl

and getlabel' n Cx s ((l,n',s').r) & (s = s' & (n' = -1 | n' > n - 4000 )) = l,Cx
 || getlabel' n Cx s ((l,n',s').r) = getlabel' n Cx s r
 || getlabel' n Cx s [] = let l = idlit ("PC"@itos n@":"@s) in l,((l,-1,s).Cx)

and getlabel n Cx s = getlabel' n Cx s Cx

and callfun fun dst src1 src2 = [Mmove (reg 0) pushV; Mmove src1 pushV; Mmove src2 pushV; Mcall fun;
				 Mmove (reg 0) dst; Mmove popV (reg 0)]

and fixCx n Cx = let new,old = partition (\(l,p,s). p = (-1)) Cx
                 in let use,_ = partition (\(l,p,s). p > n - 4000) old  -- remove dead
		 in let rnew = rev new
                 in let n',new' = mapstate (\n.\(l,_,s). n+4,(l,n,s)) n rnew
                 in concmap (\(idlit l,_,s).[Mlabel (fst(splitat ':' l));Mword (glob s)]) rnew,n',(new'@use)

and ins n Cx ((M as Mmove (const c) (reg _)).r) =
	M . ins (n+4* aconst c) Cx r

 ||  ins n Cx ((M as Mmove (idlit s) (a2 as reg _)).r) =
        let l,Cx' = getlabel n Cx s
	in Mmove l a2. ins (n+4) Cx' r

 ||  ins n Cx ((M as Mmove (a1 as reg _) (glob s)).r) =
        let l,Cx' = getlabel n Cx s
	in 
          case a1
          in dtmp1 : [Mmove l dtmp2 ; Mmove a1 dtmp2ind]
          ||   _   : [Mmove l dtmp1 ; Mmove a1 dtmp1ind]
	  end @  ins (n+8) Cx' r

 ||  ins n Cx ((M as Mmove (glob s) (a2 as reg _)).r) =
        let l,Cx' = getlabel n Cx s
	in
          case a2
          in dtmp1 : [Mmove l dtmp2 ; Mmove dtmp2ind a2]
          ||   _   : [Mmove l dtmp1 ; Mmove dtmp1ind a2]
	  end @  ins (n+8) Cx' r

 ||  ins n Cx ((M as Mmove _ _).r) =
	M . ins (n+4) Cx r

 ||  ins n Cx ((M as Mop2 div a1 a2).r) = callfun "_emuldiv" a2 a2 a1 @ ins (n+24) Cx r
 ||  ins n Cx ((M as Mop2 mod a1 a2).r) = callfun "_emulmod" a2 a2 a1 @ ins (n+24) Cx r
 ||  ins n Cx ((M as Mop2 op a1 a2).r) =	 M . ins (n+4) Cx r
 ||  ins n Cx ((M as Mop3 div a1 a2 a3).r) = callfun "_emuldiv" a3 a2 a1 @ ins (n+24) Cx r
 ||  ins n Cx ((M as Mop3 mod a1 a2 a3).r) = callfun "_emulmod" a3 a2 a1 @ ins (n+24) Cx r
 ||  ins n Cx ((M as Mop3 op a1 a2 a3).r) = M . ins (n+4) Cx r

 ||  ins n Cx ((M as Mcall s).r) = M . ins (n+8) Cx r
 ||  ins n Cx ((M as Mjumpf s).r) = let t,n',Cx' = fixCx (n+4) Cx
                                 in M . t @ ins n' Cx' r
 ||  ins n Cx ((M as Mjump s).r) = let t,n',Cx' = fixCx (n+4) Cx
                                in M . t @ ins n' Cx' r

 ||  ins n Cx ((M as Mreturn).r) = let t,n',Cx' = fixCx (n+12) Cx
                                in M . t @ ins n' Cx' r

 ||  ins n Cx ((M as Mcompare a1 a2).r) = M . ins (n+4) Cx r
 ||  ins n Cx ((M as Mjcond cc s).r) = M . ins (n+4) Cx r
 ||  ins n Cx ((M as Mboolcc cc a1).r) = M . ins (n+8) Cx r

 ||  ins n Cx ((M as m as Mcalltag t r1).r) = M . ins (n+20) Cx r
 ||  ins n Cx ((M as m as Mjumptag t r1).r) = let t,n',Cx' = fixCx (n+4) Cx
                                           in M . t @ ins n' Cx' r
 || ins n Cx ((M as Mcase a l h _ ls x).r) =
    M . ins (n+20 + 4*length ls + (if l=0 then 0 else 1)) Cx r
 || ins n Cx ((M as Mdata).r) = let d,i = splitat Mtext r     -- skip data
                                in M . d @ (Mtext. ins (align n) Cx i)    -- align is a lie, but ..

|| ins n Cx ((M as Mnoop).r) = M . ins n Cx r
|| ins n Cx ((M as Mtext).r) = M . ins (align n) Cx r      -- align is a lie, but probably correct
|| ins n Cx ((M as Mword (glob  a)).r) = M . ins (n+4) Cx r
|| ins n Cx ((M as Mword (idlit a)).r) = M . ins (n+4) Cx r
|| ins n Cx ((M as Mword (const i)).r) = M . ins (n+4) Cx r
|| ins n Cx ((M as Mdfloat s).r) = M . ins (n+8) Cx r
|| ins n Cx ((M as Mstring s).r) = M . ins (n+length s) Cx r

|| ins n Cx ((M as Mexport a).r) = M . ins n Cx r
|| ins n Cx ((M as Mcom s).r) = M . ins n Cx r
|| ins n Cx ((M as Mlabel l).r) = M . ins n Cx r
|| ins n Cx ((M as Masm s l).r) = let i = itlist (\c.\n.if c = '\n' then n+1 else n) s 0 -- !!!?
                                in M . ins (n+4*i) Cx r
|| ins n Cx ((M as Malign).r) = M . ins (align n) Cx r
|| ins n Cx ((M as Mfunbegin s _).r) = M . ins n Cx r
|| ins n Cx ((M as Mfunend ).r) = let t,n',Cx' = fixCx n Cx
                                in M . t @ ins n' Cx' r
|| ins n Cx (M.r) = M . ins (n+4) Cx r   -- ?? did not understand
|| ins n Cx [] = let t,_,_ = fixCx n Cx
                 in t

and mtrans3 m = ins 0 [] m

end
