module
#include "../expr/einfo_t.t"
#include "../expr/id_t.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Eprint.t"
#include "../misc/setofid.t"
#include "Genv.t"
#include "Gutil.t"
#include "Gseq.t"
#include "Gfreeset.t"
#include "Gmode_t.t"
#include "Gcodedef_t.t"
#include "Gcode.t"
/*import G: Gmode->Expr->Genv->Int->Glabel->Int ->
		(Glabel->((List Gcode)#Glabel));*/

export Gtail, Gtail2;
rec

   	vareq (mkid n1 _ _ _) (Eidapl (mkid n2 _ _ _) []) = n1 = n2
 ||	vareq (mkid n1 _ _ _) (Einfo _ (Eidapl (mkid n2 _ _ _) [])) = n1 = n2
 ||	vareq _ _ = false

and	Gtail3 (i.il) (e.el) (s.sl) pos r n =
	   if vareq i e then
		Gtail3 il el sl (pos+1) r n
	   else
		Gseq [	G (C(n)) e r n Notalabel 0;
			if Imem i s then
				Gseq [	Gtail3 il el sl (pos+1) r (n+1);
					Gi [ MOVE (n-pos+1) ]
				]
			else
				Gseq [	Gi [ MOVE (n-pos+1) ];
					Gtail3 il el sl (pos+1) r n
				]
		]

 ||	Gtail3 _  [] sl _ _ _ = Gi []
 ||	Gtail3 il el sl _ _ _ = fail "Gtail3"
and	Gtail2 el r n =
		let rel = reverse el  in
		let nel = head n rel  in
		let iil = listenv r n in
		let (_.sl) =
			itlist
			(\(i,e).\(s.sl).
			if vareq i e then
				s.s.sl
			else
				Iu (Gfreeset e) s.s.sl)
			(combine(iil, nel))
			[[]]
			
		and m = length el
		in
		if n > m then
	 		Gseq [	Gtail3 iil rel sl 1 r n;
				Gi [ POP (n-m) ]
			]
		else
 			Gseq [	Gcpushrev (head (m-n) el) r n;
				Gtail3 iil nel sl 1 r m
			]
and	Gtail (Eidapl f [].el) r n =
		case lookenv r f in
		  0: Gseq [ Gtail2 el r n;
		  	    Gi [ PUSHGLOBAL f ]
		     ]
		||_: Gtail2 (Eidapl f [].el) r n
		end
||      Gtail es _ _ = fail ("Gtail "@show_list pr es)
end
