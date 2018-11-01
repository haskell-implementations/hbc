module
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/einfo_t.t"
#include "../expr/constr_t.t"
#include "../expr/constrfun.t"
#include "../ExprE/Eprint.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Eutil.t"
#include "../misc/flags.t"
#include "../funnos.h"
#include "Gcodedef_t.t"
#include "Gmode_t.t"
#include "Gseq.t"
#include "Genv.t"
#include "Gcase.t"
#include "Gtail.t"
#include "Gutil.t"
#include "Gcode.t"
export SG;
rec
   SG mode (Efailmatch _) _ _ Notalabel _ _ _ = fail "SG,failmatch,notalabel"
|| SG mode (Efailmatch _) r n fl fs _ _ = Gseq [ Gpop (n-fs); Gi [JMP fl] ]

|| SG (SE s) (Einfo (doeval is) e) r n fl fs nstart lr =
		Gseq [	SGdoeval is r n nstart lr s;
			SG (SE s) e r n fl fs nstart lr
		     ]

|| SG (SE s) (Eidapl (mkid Fseq _ (idi_var (var_pre _) _ _) _) [e1;e2]) r n fl fs nstart lr=
		Gseq [  SG (SE s) e1 r n fl fs nstart lr;
			Gi [ POP 1 ];
			SG (SE s) e2 r n fl fs nstart lr
		]

|| SG (SE s) (e as Eidapl (mkid pn _ (idi_var (var_pre _) _ _) _) el) r n fl fs nstart lr =
		Gseq [	SG (SB s) e r n Notalabel 0 nstart lr;
			Gi [ BCONSTR (prebasicconstr pn) ]
		]

|| SG (SE s) (Einfo noeval (Eidapl i [])) r n fl fs nstart lr=
		pushvar i r n s

|| SG (SE s) (Eidapl i []) r n fl fs nstart lr=
		Gu 1 (\[l]. 
		Gseq [	pushvar i r n s;
			Gi [ PUSH 0 ];
			Gi [ TEVAL ];
			Gi [ JTRUE l ];
			Gi [ POP (s - (nstart - 1)) ];
			Gi [ JMP lr ];
			Gi [ LABEL l ]
		]
                )

|| SG (SE s) (Eidapl i el) r n fl fs nstart lr & (~id_is_predef i) =
		Gseq [	Gi [POP (n - nstart)];
			Gi [JMP lr]
		     ]

|| SG (SE s) (Elet re d e) r n fl fs nstart lr=
		Gseq [  Gi [ POP (n - nstart) ];
			Gi [ JMP lr ]
		     ]

|| SG (SE s) (Econstr c []) r n fl fs nstart lr=
		G (C s) (Econstr c []) r n fl fs 

|| SG (SE s) (Econstr c el) r n fl fs nstart lr=
		Gseq [  Gi [POP (n - nstart)];
			Gi [JMP lr]
		     ]

|| SG (SE s) (Einfo _ e) r n fl fs nstart lr = SG (SE s) e r n fl fs nstart lr

|| SG (SE s) (Ecase e pl de) r n fl fs nstart lr=
                let m = length pl in
                Gu (m+2) (\(L1.L2.Ls).
		let clls = (map2 (\(c,x,_).\l.(cno c,length x,l)) pl Ls) in
		let d = (if cnoc pl = m then Notalabel else L1) in
		Gseq [  SG (SE n) e r n fl fs nstart lr;
			Gi [ PUSH 0 ];
			Gi [ GETTAG ];
			Gi [ CASE (cnoc pl) clls d ];
			Gseq (map2
				(\(c,is,e1).\l.
				let (r2,n2) = addenv r (reverse
					             (filter isndg is)) (n+1)
				in
				   Gseq [  Gi [ LABEL l ];
				           Gi [ SPLIT (map isndg is)
                                                      (cno c) (length is) ];
					   SG (SE s) e1 r2 n2 L1 (n+1) nstart lr;
					   Gi [ JMP L2 ]
                                        ]
                                   ) pl Ls);
			if de ~= Efailmatch 0 then
       			   Gseq [  Gi [ LABEL L1 ];
				   SG (SE s) de r (n+1) fl fs nstart lr
				]
			else
			   Gi [];
                           Gi [ LABEL L2 ]
		     ]
                     )

|| SG (SB s) (Einfo (doeval is) e) r n fl fs nstart lr =
		Gseq [	SGdoeval is r n nstart lr s;
			SG (SB s) e r n fl fs nstart lr
		     ]

|| SG (SB s) (Eidapl (mkid pn _ _ _) [e1;e2]) r n fl fs nstart lr & (bigop pn)=
		Gseq [	Gi [ POP (n - nstart) ];
			Gi [ JMP lr ]
		]

|| SG (SB s) (Eidapl (mkid pn _ _ _) [e1;e2]) r n fl fs nstart lr & (lstrop pn)=
		Gseq [	Gi [ POP (n - nstart) ];
			Gi [ JMP lr ]
		]

|| SG (SB s) (Elet re d e) r n fl fs nstart lr=
		Gseq [  Gi [ POP (n - nstart) ];
			Gi [ JMP lr ]
		     ]

|| SG (SB s) (Eidapl (mkid pn ps (idi_var (var_pre _) _ _) _) el) r n fl fs nstart lr=
		Gseq [	Gseq (map (\e.SG (SB n) e r n Notalabel 0 nstart lr) el);
			Gi [ BASICOP (prebasicarg pn) (prebasicconstr pn) (prebasicop pn) ];
			Gpop (n-s)
		]

|| SG (SB s) (Econstr c []) r n fl fs nstart lr=
		Gseq [	Gi [ PUSHBASIC (GvInt (cno c)) ];
			Gpop (n-s)
		]

|| SG (SB s) (Einfo noeval (Eidapl i [])) r n fl fs nstart lr=
		Gseq [	pushvar i r n s;
			Gi [ GET Gbint ]
		]

|| SG (SB s) (Einfo _ e) r n fl fs nstart lr = SG (SB s) e r n fl fs nstart lr

|| SG (SB s) e r n fl fs nstart lr=
	       Gseq [ SG (SE s) e r n fl fs nstart lr;
                      Gi [ GET Gbint ]
                    ]

|| SG _ e _ _ _ _ _ _ = fail ("Stingy Gcode "@pr e)

and SGdoeval is r n nstart lr s =
	Gseq (map (\i.
		Gu 1 (\[l]. 
		Gseq [	pushvar i r n s;
			Gi [ TEVAL ];
			Gi [ JTRUE l ];
			Gi [ POP (s - nstart) ];
			Gi [ JMP lr ];
			Gi [ LABEL l ]
		]
                )) is)
end
