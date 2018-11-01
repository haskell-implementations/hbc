module
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/constr_t.t"
#include "../expr/constrfun.t"
#include "../expr/einfo_t.t"
#include "../ExprE/Expr_t.t"
#include "../misc/flags.t"
#include "Genv.t"
#include "Gseq.t"
#include "Gutil.t"
#include "Gmode_t.t"
#include "Gcodedef_t.t"
#include "Gcode.t"
/*import G: Gmode->Expr->Genv->Int->Glabel->Int ->
		(Glabel->((List Gcode)#Glabel));*/

export Gcase,isndg,cnoc,anyused,isflat;
rec isndg e = ~isdummy e | Gflag
and isflat' (Cconstr _ _ (mktinfo _ _ _ x _ _ _ _) _ _ _) = x
and isflat ((c,_,_)._) = isflat' c
||  isflat _ = false
and cnoc ((c,_,_)._) = nconstrs c
and lastcno pl = case last pl in (c,_,_) : cno c+1 end
and anyused = exists (\(_,is,_).exists isndg is)
and
    Gcase mode e pl de r n fl fs =
 	let m = length pl  in
	Gu (m+2) (\(L1.L2.Ls).
	let needpush = anyused pl in
	let n1 = if needpush then n+1 else n in
	let flat = isflat pl in
	let clls = (map2 (\(c,x,_).\l.(cno c,length x,l)) pl Ls) in
	let ls = if needpush then [] else map (\(c,x,l).(c, (0,l))) (filter (\(_,x,_).x=0) clls) in
	let lim = case e in Einfo limitok _ : lastcno pl || _ : cnoc pl end in
	let d = if lim = m then Notalabel else L1 in
	Gseq [
		Gseq
		[ Gi (if flat then [AMODE Astack] else []);
		  G (J n ls) e r n Notalabel 0;
		  Gi (if needpush then [PUSH 0] else []);
		  Gi (if flat then [GET Gbint; AMODE Aheap] else [GETTAG]);
		  Gi [ CASE lim clls d ]
		];
		-- Case list
		Gseq 
		(map2
		(\(c,is,e1).\l.
		let (r2,n2) = addenv r (reverse (filter isndg is)) n1  
		in
		    Gseq [ Gi [ LABEL l];
			   if needpush then
				Gi [SPLIT (map isndg is) (cno c) (length is)]
			   else
				Gi [];
			   G mode e1 r2 n2 L1 n1;
			   Gjmp mode L2
			]) pl Ls);
		-- Default case
		if de ~= Efailmatch 0 then
			Gseq [	Gi [ LABEL L1 ];
				G mode de r n1 fl fs
			]
		else
			Gi [];
		Gi [ LABEL L2 ]
	]
	)

end
