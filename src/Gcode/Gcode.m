module
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/ttype.t"
#include "../expr/einfo_t.t"
#include "../expr/constr_t.t"
#include "../expr/constrfun.t"
#include "../ExprE/Eprint.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Eutil.t"
#include "../misc/flags.t"
#include "../misc/misc.t"
#include "../funnos.h"
#include "Gcodedef_t.t"
#include "Gmode_t.t"
#include "Gseq.t"
#include "Genv.t"
#include "Gcase.t"
#include "Gconst.t"
#include "Gtail.t"
#include "Gutil.t"
#include "Gid.t"
#include "Gprint.t"

export G;
rec
   G _ (e as Eidapl i _) _ _ _ _ & (id_ismethod i) =
       fail ("method application "@pr e)

|| G _ (e as Eidapl (mkid _ _ (idi_var (var_dict _) _ _) _) (_._)) _ _ _ _ =
       fail ("dictionary application "@pr e)

|| G mode (Ecase e pl de) r n fl fs = Gcase mode e pl de r n fl fs

|| G mode (Efailmatch _) _ _ Notalabel _ = fail "G,failmatch,notalabel"
|| G mode (Efailmatch _) r n fl fs = Gseq [ Gpop (n-fs); Gi [JMP fl] ]

|| G mode (ee as Elet re d e) r n fl fs =
		let (r2,n2) = addenv r (map (\(i,_).i) d) n  in
		let m = n2-n in
		if re then
#if 0
                  if exists (isvar o snd) d then
                    fail ("G:black hole "@pr ee)
                  else
#else
		-- replace a single variable with an application of the identity function to it
		let d = mapsnd (\ e . if isvar e then Eidapl identityfun [e] else e) d in
#endif
		    (\t.
		    let (Gs, t') = Gseql (map (\(_,e). G (C n2) e r2 n2 Notalabel 0) d) t
		    in  Gseq [ (if RecBlock & all cleanG Gs then Gi [RECBLOCK Gs] else flatrecblock Gs m);
			       G mode e r2 n2 fl fs
			     ] t'
                    )
#if 0
		    Gseq [ Gi [ ALLOC m ];
		    	   Gseq (map2 (\(_,e).\x.
					Gseq [ G (C n2) e r2 n2 Notalabel 0;
					       Gi [ UPDATE ??? (m-x) ]])
				     d (from 0));
			   G mode e r2 n2 fl fs
		    ]
#endif
		else
		    Gseq [ Gseq (map2 (\(_,e).\x.G (C(n+x)) e r (n+x) Notalabel 0) d (from 0));
		    	   G mode e r2 n2 fl fs
		    ]

|| G m (Einfo vectordef e) r n fl fs = 
		Gseq [	Gi [ ANNOT "VECTORDEF" ];
			G m e r n fl fs
		     ]

|| G m (Einfo (spark is) e) r n fl fs = G m e r n fl fs

|| G m (Einfo (doeval is) e) r n fl fs =
		Gseq [	Gdoeval is r n;
			G m e r n fl fs
		     ]

|| G R e r n fl fs & (isconstant r e & ConstReturnCaf) =
		Gseq [  Gi [ CONSTBLOCK (Gconst e) ];
		        Gslide n;
		        Gi [
			    EVAL [];
			    UPDATE Gbother 1; 
			    UNWIND
			   ]
		   ]

|| G R (Einfo (noarrow t) (Einfo noeval (Eidapl i []))) r n fl fs =
		Gseq [	pushvar i r n 0;
			Gi [ UPDATE (updtype t) 1; RET ]
		]

|| G R (Einfo noeval (Einfo (noarrow t) (Eidapl i []))) r n fl fs =
		Gseq [	pushvar i r n 0;
			Gi [ UPDATE (updtype t) 1; RET ]
		]

|| G R (Einfo (noarrow t) (Eidapl i [])) r n fl fs =
		Gseq [	pushvar i r n 0;
			if gnoargs i <= 0 then
				Gi [ EVAL [] ]
			else
				Gi [];
			Gi [ UPDATE (updtype t) 1; RET ]
		]

|| G R (Einfo noeval (Eidapl i [])) r n fl fs =
		Gseq [	pushvar i r n 0;
			Gi [ UPDATE Gbother 1; UNWIND ]
		]

|| G R (Eidapl i []) r n fl fs =
		Gseq [	pushvar i r n 0;
			if gnoargs i <= 0 then
				Gi [ EVAL [] ]
			else
				Gi [];
			Gi [ UPDATE Gbother 1; UNWIND ]
		]

|| G R (Eidapl (mkid Fseq _ _ _) [e1;e2]) r n fl fs =
		Gseq [  G (E n) e1 r n fl fs;
			Gi [ POP 1 ];
			G R e2 r n fl fs
		]


|| G R (Eidapl (id as mkid Fleftpat _ _ _) [Econstr c1 el1; Econstr c2 el2]) r n fl fs =
        let nel = length el1 in
	let is_global (Eidapl id []._) = id_is_global id
	||  is_global _ = false in
		Gseq [  Gcpushrev (el1 @ el2) r n;
		        if is_global el2 then
			    Gi (conc (rept nel [  PUSHGLOBAL identityfun; 
						  MKAP (idtopstr identityfun); 
					          UPDATE Gbother nel
					       ]))
			else
			    Gi (conc (rept nel [  UPDATEINDIR nel ]));
		        Gi [POP (nel+n); PUSH 1; EVAL []; UPDATE Gbother 1; RET]
		]

|| G R (Eidapl (id as mkid idnr _ _ _) [e]) r n fl fs & (idnr >= PBSELBASE) =
                Gseq [  Gi [PUSH 0];    -- To get the stack set up for leftpat
		        G (C n) e r n fl fs;
		        Gi [EVAL []; UPDATE Gbother 1; UNWIND]
                     ]

-- All metcalls are considered to have the correct arity
|| G R (Einfo metcall (Eidapl i el)) r n fl fs =
	Gseq [	Gtail2 el r n;
		Gi [ JGLOBAL (gnoargs i) i ]
	]

|| G R (Einfo vecreg2 e) r n fl fs =
	Gseq [	Gi [ ANNOT "REG2";
		     POP 1 ];
		G R e r (n-1) fl fs
	     ]

#if 1
|| G R (e as Eidapl mi el) r n fl fs & (InlineCallMet & iscallmethod mi) =
	case el in
	   (e.(ec as Econstr c _).es) :
		Gseq [	Gtail2 (e.es) r n;
			Gi [ EVAL [];
			     SPLITPAIR true true;
			     JMETHOD (length es) (cno c) ]
		]
	|| _ : fail ("G-callmethod "@pr e)
	end
#endif

|| G R (Eidapl i el) r n fl fs & (~id_is_predef i) =
	let nel = length el in
	let na = gnoargs i in
	if na = nel then
		Gseq [	Gtail2 el r n;
			Gi [ JGLOBAL nel i ]
		]
	else if na = -1 | na > nel then
		Gseq [	Gtail (Eidapl i [].el) r n;
			Gi [ JFUN nel ]
		]
	else -- too many args supplied
#if 0
		let d = nel-na in
		Gseq [	Gtail2 el r n;
			Gi [ ALLOC 1 ]; 
			Gi (rept (d+1) (PUSH (nel+1)));
			Gi [ PUSH (d+1) ];
			Gi (conc (rept d [ MKAP; PUSH 0; MOVE (nel+4) ]));
			Gi [ UPDATE Gbother 1; POP 1; MOVE (na+1) ];
			Gi [ JGLOBAL na i ]
		]
#else
		Gseq [	Gtail (Eidapl i [].el) r n;
			Gi [ JFUN nel ]
		]
#endif

|| G R (Einfo _ e) r n fl fs = G R e r n fl fs

|| G R e r n fl fs =
		Gseq [	G (E n) e r n fl fs;
			Gi [ UPDATE Gbother (n+1); POP n; UNWIND ]
		]
	
|| G (E s) (Eidapl (mkid Fseq _ _ _) [e1;e2]) r n fl fs =
		Gseq [  G (E s) e1 r n fl fs;
			Gi [ POP 1 ];
			G (E s) e2 r n fl fs
		]

|| G (E s) (Eidapl (mkid ix _ _ _) [e1;e2]) r n fl fs & (ix = Frindex | ix = Frindexu) =
		Gseq [  G (E n) e1 r n fl fs;
			G (B (n+1) Gbint) e2 r (n+1) fl fs;
		        Gi ([ BASICOP Gbint Gbother INDEX ] @ if ix = Frindex then [ EVAL [] ] else []);
		        Gslide (n-s)
		]

|| G (E s) (e as Eidapl (mkid pn _ (idi_var (var_pre _) _ _) _) el) r n fl fs =
		let p = prebasicconstr pn in
		let a = prebasicarg pn in
		Gseq [	G (B s a) e r n Notalabel 0;
			Gi [ BCONSTR p ]
		]

|| G (E s) (Einfo noeval (Eidapl i [])) r n fl fs =
		pushvar i r n s

|| G (E s) (Eidapl i []) r n fl fs =
		Gseq [	pushvar i r n s;
			Gi [ EVAL [] ];
			updstack i r s
		]

#if 1
|| G (E s) (e as Eidapl mi el) r n fl fs & (InlineCallMet & iscallmethod mi) =
	case el in
	   (e.(ec as Econstr c _).es) :
		Gseq [	Gi [ ALLOC 1 ];
			Gcpushrev (e.es) r (n+1);
			Gi [ EVAL [];
			     SPLITPAIR true true;
			     CALLMETHOD (length es) (cno c) [] ];
			Gslide (n-s)
		]
	|| _ : fail ("G-callmethod "@pr e)
	end
#endif

|| G (E s) (Eidapl i el) r n fl fs & (~id_is_predef i) =
        let k = length el in
	if gnoargs i = k then
		Gseq [	Gi [ ALLOC 1 ];
			Gcpushrev el r (n+1);
			Gi [ CALLGLOBAL k i [] ];
			Gslide (n-s)
		]
	else
		Gseq [	Gi [ ALLOC 1 ];
			Gcpushrev (Eidapl i [].el) r (n+1);
			Gi [ CALLFUN k [] ];
			Gslide (n-s)
		]

|| G (E s) (Econstr c el) r n fl fs =
		G (C s) (Econstr c el) r n fl fs

|| G (E s) (Einfo _ e) r n fl fs = G (E s) e r n fl fs

|| G (j as (J s ls)) (Einfo strict e) r n fl fs = G j e r n fl fs

|| G (J s ls) (e as Econstr c []) r n fl fs =
		case assocdef (cno c) ls (-1,Notalabel) in
		   (-1,Notalabel) : G (E s) e r n fl fs
		|| (k, l) :
			Gseq[	Gpop (n-s);
				Gi [ JMP l ]
			]
		end
			
|| G (J s ls) e r n fl fs =
		G (E s) e r n fl fs

|| G (B s _) (Eidapl (mkid Fcno _ _ _) [e1]) r n fl fs =
		Gseq [	G (E n)     e1 r  n    fl fs;
		        Gi [ GETTAG ];
			Gpop (n-s)
		     ]
|| G (B s _) (Eidapl (mkid pn _ _ _) [e1;e2]) r n fl fs & (bigop pn) =
		Gseq [	G (E n)     e1 r  n    fl fs;
			G (E (n+1)) e2 r (n+1) fl fs;
			Gi [ BIGOP Gbother (prebasicop pn) ];
			Gpop (n-s)
		]

|| G (B s _) (Eidapl (mkid pn _ _ _) [e1;e2]) r n fl fs & (lstrop pn) =
		Gseq [	G (E n)     e1 r  n    fl fs;
			G (E (n+1)) e2 r (n+1) fl fs;
			Gi [ BIGOP Gbstring (prebasicop pn) ];
			Gpop (n-s)
		]

|| G (B s _) (Eidapl (mkid pn ps (idi_var (var_pre _) _ _) _) el) r n fl fs & (pn ~= Frindex & pn ~= Frindexu ) =
		let p = prebasicconstr pn in
		let a = prebasicarg pn in
		Gseq [	Gseq (map (\e.G (B n a) e r n Notalabel 0) el);
			Gi [ BASICOP a p (prebasicop pn) ];
			Gpop (n-s)
		]

|| G (B s _) (Econstr c []) r n fl fs =
		Gseq [	Gi [ PUSHBASIC (if isdfloat c then (GvDFloat (stof (cname c))) 
					else if issfloat c then (GvSFloat (stof (cname c))) 
					else (GvInt (cno c))) ];
			Gpop (n-s)
		]
		
|| G (B s p) (Einfo noeval (Eidapl i [])) r n fl fs =
		Gseq [	pushvar i r n s;
			Gi [ GET p ]
		]

|| G (B s p) (Eidapl i []) r n fl fs =
		Gseq [	pushvar i r n s;
			Gi [ EVAL [] ];
			updstack i r s;
			Gi [ GET p ]
		]

|| G (B s p) (Einfo _ e) r n fl fs = G (B s p) e r n fl fs

|| G (B s p) e r n fl fs =
		Gseq [	Gi [ AMODE Astack ];
			G (E s) e r n fl fs;
			Gi [ AMODE Aheap; GET p ]
		]

|| G (C s) e r n fl fs & (isconstant r e & ConstCaf) =
	Gseq [	Gi [CONSTBLOCK (Gconst e)];
		Gslide (n-s)
	     ]

|| G (C s) (Einfo strict e) r n fl fs =
		-- switch to strict evaluation
		G (E s) e r n fl fs

|| G (C s) (Eidapl i []) r n fl fs =
		pushvar i r n s

|| G (C s) (Eidapl (id as mkid idnr _ _ _) el) r n fl fs & (idnr >= PBSELBASE) =
                G (C s) (Eidapl pbselfun el) r n fl fs

|| G (C s) (e as Eidapl (mkid _ _ (idi_method _ _ _) _) _) r n fl fs =
        fail ("Construct method call "@pr e)

|| G (C s) (Eidapl i el) r n fl fs & (~id_is_predef i) =
	let lel = length el in
	let ar = gnoargs i in
	if Stingy & ar = lel & ~ all Eisconst el then
                Gseq [  Gcpushrev el r n; 
                        Gi [ SCALLGLOBAL ar i ]
		     ]
        else
	if ar ~= -1 & ar ~= 0 & ar <= lel & ~NoVector then
		Gseq [	Gcpushrev el r n;
			Gi [ MKAPLV i ar ];
			Gi (rept (lel-ar) (MKAP (idtopstr i)));
			Gslide (n-s)
		]
	else
		Gseq [	Gcpushrev (Eidapl i [].el) r n;
			Gi (rept lel ((if ar > 0 & lel < ar then MKCAP else MKAP) (idtopstr i)));
			Gslide (n-s)
		]

|| G (C s) (Econstr c el) r n fl fs =
		Gseq [	Gcpushrev el r n;
			Gi [ CONSTR (cname c, typenamefromtinfo (ctinfo c)) (constrtype c) (cno c) (length el)];
			Gslide (n-s)
		]

|| G (C _) (Ecfunction b i) r n fl fs = Gi [ PUSHCFUNCTION b (idtostr i) ]

|| G mode (Einfo _ e) r n fl fs = 
		G mode e r n fl fs

|| G _ (Eidapl (id as mkid idnr _ _ _) [e]) r n fl fs & (idnr >= PBSELBASE) = fail "PBsel not in C scheme"
|| G _ (Eidapl (id as mkid Fleftpat _ _ _) el) r n fl fs = fail "leftpat not in the R compilation scheme"

|| G _ e _ _ _ _ = fail ("Gcode "@pr e)

and flatrecblock Gs m =
		    Gseq [ Gi [ ALLOC m ];
			   Gi (concmap2 (\g.\x.g @ [ UPDATE Gbother (m-x) ]) Gs (from 0)) ]
and isvar (Eidapl _ []) = true
||  isvar (Einfo _ e) = isvar e
||  isvar _ = false
and iscallmethod i = let n = id_no i in n > Fcallmethod & n < Fcallmethod + 50
and updtype t = 
         if eqtype t Tint    then Gbint
    else if eqtype t Tchar   then Gbchar
    else if eqtype t Tdfloat then Gbdfloat
    else if eqtype t Tsfloat then Gbsfloat
    else Gbother
end
