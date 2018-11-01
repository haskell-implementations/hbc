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
#include "machine.tt"
export mbasicop;
rec
   commutes add = true
|| commutes mul = true
|| commutes dfadd = true
|| commutes dfmul = true
|| commutes sfadd = true
|| commutes sfmul = true
|| commutes btand = true
|| commutes btor = true
|| commutes btxor = true
|| commutes _   = false
and mop3 op a b c & (b = c) = Mop2 op a b
||  mop3 op a b c & (a = c & commutes op) = Mop2 op b a
||  mop3 op a b c = Mop3 op a b c
and
   mbasicop svf t g G pa p gop aV S & (mem gop [NEG; COMPL; FTOI; ITOF; SFTODF; DFTOSF]) =
	let (a.V) = aV in
        let greg = if p = Gbsfloat | p = Gbdfloat then gfreg else gdreg in
	let op = case gop in 
                    NEG : if p = Gbsfloat then sfneg else if p = Gbdfloat then dfneg else neg 
                 || COMPL : btcompl 
                 || FTOI : if pa = Gbsfloat then sftoi else dftoi
                 || ITOF : if p = Gbsfloat then itosf else itodf
                 || SFTODF : sftodf
                 || DFTOSF : dftosf
                 end in
	let dr = reg (greg aV S) in
	let rec (c, d, u.uv, us, k) = M svf t g G V1 S
	and    (c1, V1) = 
			case u in
			  Wpush: (Mop2 op a pushV.c, popV.V)
			||Wuse : (Mop2 op a dr.c, dr.V)
			end
	in (Mcom (strop gop).c1, d, Wuse.uv, us, k)

|| mbasicop svf t g G ap p INDEX bV aS =
        let (a.S) = aS
        and (b.V) = bV in
        let br = reg (gdreg V aS) in
	let rec (c, d, uv, u.us, k) = M svf t g G V (ar.S)
        and (cr, r) = if u=Wreg 0 then ([Mmove a (reg 0)], 0) else intoareg a (br.V) S
        and ar = reg r
	in  (Mcom "INDEX".
             Mmove b br.				-- get index
	     cr @
	     Mmove (regind r 2) ar.			-- get the DVEK (r points to a TAG)
             Madda br ar.
	     Mmove (regind r 2) ar.			-- get element (2 skips DVEK&size)
	     c, d, Wuse.uv, Wuse.us, k)

|| mbasicop svf tt g G ap p SQR aV S =
	let (a.V) = aV in -- must be lazy!
	let move = if ap = Gbsfloat then Mmovesf else if ap = Gbdfloat then Mmovedf else Mmove in
	let rgreg = if p = Gbsfloat | p = Gbdfloat then gfreg else gdreg in
	let op' = if p = Gbsfloat then sfmul else if p = Gbdfloat then dfmul else mul in
	let dr = reg (rgreg V S) in
	let rec (c, d, u.uv, us, k) = M svf tt g G V1 S
	and    (c1, V1) =
	    if use3op then
		case u in
		  Wpush: (Mop3 op' a a pushV.c, popV.V)
		||Wuse : (Mop3 op' a a dr.c, dr.V)
		end
	    else
		case u in
		  Wpush: (move a pushV.
			  Mop2 op' a (Vind 0).c, popV.V)
		||Wuse : if a=dr then
				(Mop2 op' a dr.c, dr.V)
			 else
				(move a dr.Mop2 op' dr dr.c, dr.V)
		end
	in (Mcom "SQR".c1, d, Wuse.uv, us, k)
	

|| mbasicop svf tt g G ap p op abV S =
	let (a.b.V) = abV in -- must be lazy!
	    let (agreg, move, comp, fc) = if ap = Gbsfloat then (gfreg, Mmovesf, Mcomparesf, sfcc) else if ap = Gbdfloat then (gfreg, Mmovedf, Mcomparedf, dfcc) else (gdreg, Mmove, Mcompare, (\x.x)) in
	    let rgreg = if p = Gbsfloat | p = Gbdfloat then gfreg else gdreg in
	    if isbinarithop op then
		let op' = mgop p op in
		let dr = reg (rgreg V S) in
		let rec (c, d, u.uv, us, k) = M svf tt g G V1 S
		and    (c1, V1) =
		    if use3op then
			case u in
			  Wpush: (Mop3 op' a b pushV.c, popV.V)
			||Wuse : (mop3 op' a b dr.c, dr.V)
			end
		    else
			case u in
			  Wpush: (move b pushV.
				  Mop2 op' a (Vind 0).c, popV.V)
			||Wuse : if b=dr then
					(Mop2 op' a dr.c, dr.V)
				 else if a=dr & commutes op' then
					(Mop2 op' b dr.c, dr.V)
				 else
					-- This is the difficult case. We
					-- must have a temp register that
					-- does not clobber a.
					let dr = reg (rgreg (a.V) S) in
					(move b dr.Mop2 op' a dr.c, dr.V)
			end
		in (Mcom (strop op).c1, d, Wuse.Wuse.uv, us, k)
		
	    else
		let rec (c, d, u.uv, us, k) = M svf tt g G V1 S
		and    (ar, V1) = 
			case u in
			  Wpush: (pushV, popV.V)
			||Wuse : let r = reg (gdreg V S) in
					(r, r.V)
			end
		in (	
			Mcom (strop op).
			comp a b.
			Mboolcc ((fc o mopcc o ccrev o ccop)op) ar.c,
				d, Wuse.Wuse.uv, us, k)
end
