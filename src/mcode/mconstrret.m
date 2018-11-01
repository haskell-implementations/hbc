module
#include "mcodedef_t.t"
#include "../Gcode/Gcodedef_t.t"
#include "Wuse.t"
#include "mutil.t"
#include "mutil1.t"
#include "mregs.t"
#include "mstrlbl.t"
#include "mstrid.t"
#include "handmade.mcode.t"
#include "mvectable.t"
#include "../misc/flags.t"
#include "machine.tt"

export mcbasicret, mcnilret, mctagret, mcpairret, mbconstrret;
rec

local f a S greg = let r = reg(greg S []) in [ Mmove a r ], r
in
	   awayS (a as (Sind n)) S = f a S gareg
	|| awayS (a as (popS  )) S = f a S gareg
	|| awayS (a as (regind 0 m)) S = f a S gareg
	|| awayS (a as (reg 0)) S = f a S gareg
	|| awayS a S = [], a
	and
	   awayV (a as (popV))       V = f a V gdreg
	|| awayV (a as (regind 0 m)) V = f a V gdreg
	|| awayV a V = [], a
end

and
mcbasicret svf t g G bv npop V S =
	let bcs = strbv bv in
        let x   = valbv bv in
	let (move, offs) = 
		case bv in
		   GvSFloat f : (Mmovesf (fconst f), 1+sfloatsize)
		|| GvDFloat f : (Mmovedf (fconst f), 1+dfloatsize)
		|| _ : (Mmove (const x), 2)
		end in
	let (nsvf, vec) = addthemb svf (constrbv bv) in
	let (c, d, uv, us, k) = M nsvf t g G [] [] in

	    (	Mcom("CBASIC "@bcs@" "@itos x). Mcom("UPDATE "@itos(npop+1)).
		Mcom("POP "@itos npop). Mcom("RET").
	    	Mmove popV Sp.
		Mmove (Sind(-1)) (reg 0).
		Mmove (idlit bcs) (regind 0 0).
		move              (regind 0 1).
	        hpmoves vec       (regind 0 offs)@
                mreturn @
		c,
			d, allWpush, prepWuse npop allWpush, 0)

and
mcnilret svf ct t g G m npop V S =
	let (nsvf, vec) = addthem svf ct in
	let (c, d, uv, us, k) = M nsvf t g G [] [] in
	    (	Mcom("CNIL "@itos m). Mcom("UPDATE "@itos(npop+1)).
		Mcom("POP "@itos npop). Mcom("RET").
	    	Mmove popV Sp.
		Mmove (Sind(-1)) (reg 0).
		Mmove mniltag   (regind 0 0).
		Mmove (const m) (regind 0 1).
	        hpmoves vec     (regind 0 2)@
		mreturn @
                c,
			d, allWpush, prepWuse npop allWpush, 0)

and
mctagret svf ct t g G m npop V aS =
	let (nsvf, vec) = addthem svf ct in
	let (aa.S) = aS in
	let (c, d, uv, us, k) = M nsvf t g G [] [] in
	let (ca,a) = awayS aa []
	in
	    (	Mcom("CTAG "@itos m). Mcom("UPDATE "@itos(npop+1)).
		Mcom("POP "@itos npop). Mcom("RET").
	    	ca @
	    	Mmove popV Sp.
		Mmove (Sind(-1)) (reg 0).
		Mmove mtagtag   (regind 0 0).
		Mmove (const m) (regind 0 1).
		Mmove a         (regind 0 2).
	        hpmoves vec     (regind 0 3)@
		mreturn @ 
                c,
			d, allWpush, prepWuse (npop+1) allWpush, 0)

and
mcpairret svf ct t g G m npop V abS =
	let (nsvf, vec) = addthem svf ct in
	let (aa.bb.S) = abS in
	let is = mpairtag m in
	let (c, d, uv, us, k) = M nsvf t g G [] [] in
	let (ca,a) = awayS aa [ bb ] in
	let (cb,b) = awayS bb [ a ]
	in
	    (	Mcom("CPAIR"). Mcom("UPDATE "@itos(npop+1)).
		Mcom("POP "@itos npop). Mcom("RET").
	    	ca @ cb @
	    	Mmove popV Sp.
		Mmove (Sind(-1)) (reg 0).
		Mmove is (regind 0 0).
		Mmove a  (regind 0 1).
		Mmove b  (regind 0 2).
	        hpmoves vec (regind 0 3)@
		mreturn @ 
	        c,
			d, allWpush, prepWuse (npop+2) allWpush, 0)


and
mbconstrret svf t g G bc npop aV S =
	let (nsvf, vec) = addthemb svf (constrbc bc) in
	let (aa.V) = aV in
	let bcs = strbc bc in
	let (move, offs) = if bc = Gbsfloat then (Mmovesf, 1+sfloatsize) else if bc = Gbdfloat then (Mmovedf, 1+dfloatsize) else (Mmove, 2) in
	let (c, d, uv, us, k) = M nsvf t g G [] [] in
	let (ca,a) = awayV aa []
	in
	    (	Mcom("BCONSTR "@bcs). Mcom("UPDATE "@itos(npop+1)).
		Mcom("POP "@itos npop). Mcom("RET").
	    	ca @
	    	Mmove popV Sp.
		Mmove (Sind(-1)) (reg 0).
		Mmove (idlit bcs) (regind 0 0).
		move a            (regind 0 1).
	        hpmoves vec       (regind 0 offs)@
		mreturn @ 
	        c,
			d, Wuse.allWpush, prepWuse npop allWpush, 0)


end
