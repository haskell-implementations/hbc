module
#include "mcodedef_t.t"
#include "../Gcode/Gcodedef_t.t"
#include "Wuse.t"
#include "mutil.t"
#include "mutil1.t"
#include "mregs.t"
#include "mstrlbl.t"
#include "mstrid.t"
#include "mprint.t"
#include "mvectable.t"
#include "handmade.mcode.t"
export mpush, mpushglobal, mpushbasic, mpushcfunction;
rec
mpush svf t g G n V S =
	let rec (c, d, uv, u.us, k) = M svf t g G V S1
	and    (c1, S1) = newS (indS n S) u S c
	in (Mcom ("PUSH "@itos n@" "@prstk 6 S@" "@prWuses 4 (u.us)).c1, d, uv, us, k)
and
mpushglobal svf t g G i V S =
	let is = mstrid i in
	let rec (c, d, uv, u.us, k) = M svf t g G V S1
	and    (c1, S1) = newS (idlit is) u S c
	in ( Mcom ("PUSHGLOBAL "@is).c1, is.d, uv, us, k)
and
mpushcfunction :: 
	( (List String) # (List (Int#Int#Int)) # String ) ->
	Int ->
	Int ->
	(List Gcode) ->
	Bool ->
	String ->
	List Addrmode ->
	List Addrmode ->
	( (List Mcode)#(List String)#(List Wuseinfo)#(List Wuseinfo)#Int )
and
mpushcfunction svf t g G b i V S =
        let bv = GvInt 0 in
#ifdef hppa
	let is = (if b then "*C*" else "*D*") @ i in
#else
	let is = if b then i else i in
#endif
	let ts = tmplbl t in
        let (nsvf, vec) = addthemb svf (constrbv bv) in
	let rec (c, d, uv, u.us, k) = M nsvf (t+1) g G V S1
	and    (c1, S1) = newS (idlit ts) u S c
	in (	Mcom ("PUSHCFUNCTION "@is).
		Mdata.
		Mlabel ts.
		Mword (glob (strbv bv)).
		Mword (glob is).
		hpwords vec @
		Mtext.c1,
			d, uv, us, k)
and
mpushbasic svf t g G (GvInt x) V S =
	let rec (c, d, u.uv, us, k) = M svf t g G V1 S
	and    (c1, V1) = newV (const x) u V c
	in (Mcom ("PUSHBASIC "@itos x).c1, d, uv, us, k)
||
mpushbasic svf t g G (GvSFloat x) V S =
	let rec (c, d, u.uv, us, k) = M svf t g G V1 S
	and    (c1, V1) = newSF (fconst x) u V c
	in (Mcom ("PUSHBASIC "@fmtf ".9e" x).c1, d, uv, us, k)
||
mpushbasic svf t g G (GvDFloat x) V S =
	let rec (c, d, u.uv, us, k) = M svf t g G V1 S
	and    (c1, V1) = newDF (fconst x) u V c
	in (Mcom ("PUSHBASIC "@fmtf ".16e" x).c1, d, uv, us, k)
end
