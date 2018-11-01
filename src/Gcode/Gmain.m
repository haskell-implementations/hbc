module
#include "../expr/einfo_t.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../ExprE/Expr_t.t"
#include "../misc/flags.t"
#include "Genv.t"
#include "Gseq.t"
#include "Gmode_t.t"
#include "Gcodedef_t.t"
#include "Gcode.t"
#include "Stingycode.t"
#include "../funnos.h"
export Gmain;
rec Gmain :: Expr -> Gcodes
and Gmain (Emodule _ _ dss) =
        let (_, gss) = mapstate (\l.\ds.let (gs, l') = emit1 ds l in (l', gs)) (Label 1) (conc dss)
        in  gss
and emit1 (f, (Elaml I e)) =
    let r =
        let (r,n) = addenv nilenv (reverse I) 0  in
        Gtrip (f, length I)
              (G R e r n Notalabel 0)
              (if Stingy then
		   some (
                     Gu 1 (\[l].
			    Gseq [
				SG (SE n) e r n Notalabel 0 n l;
				(if n > 0 then Gseq [ Gi [ MOVE n ];
                                                      Gi [ POP (n - 1) ] ]
				 else Gseq []);
				Gi [ RET ];
				Gi [ LABEL l ];
				Gi [ MKAPLV f n ];
				Gi [ RET ]
				]))
	       else
		   Gi None
	       )
    in  if WarnOverload then 
	    case filter isdictid I in
	       [] : r
	    || is : trace ("Warning, overloaded: "@prid f) r
	    end
	else
	    r

and isdictid i = head 4 (idtostr i) = "dict"

and Gpair f g l =
    let (x, l') = f l in
    let (y, l'') = g l'
    in ((x, y), l'')
and Gtrip a f g l =
    let (x, l') = f l in
    let (y, l'') = g l'
    in ((a, x, y), l'')
and some f l = let (x, l') = f l in (Some x, l')
end
