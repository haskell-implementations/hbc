module
#include "../expr/id.t"
#include "mcodedef_t.t"
#include "../Gcode/Gcodedef_t.t"
#include "Wuse.t"
#include "mutil.t"
#include "mutil1.t"
#include "mregs.t"
#include "mstrlbl.t"
#include "mstrid.t"
#include "handmade.mcode.t"
#include "../misc/setofid.t"
#include "../misc/flags.t"
#include "mmemcheck.t"
#include "limit.h"
#include "mvectable.t"
#include "machine.tt"

export mfunstart, mfunend, msfunstart;
rec itosm m i = if i < m then itos i else "N"
and mfunstart (strs, vecs, _) t G oG i n =
    let fstr = idtopstr i in

    let nstrs = adds [fstr;"SYSTEM(Fun)";"SYSTEM(Zap)"] strs        in
    let snumf = snum fstr nstrs					    in
    let vec1  = (snum "SYSTEM(Fun)" nstrs, snumf, snumf) 	    in
    let vec2  = (snum "SYSTEM(Zap)" nstrs, snumf, snumf)	    in
    let nvecs = addv vec2 (addv vec1 vecs)                     	    in

	let (c, d, uv, us, k) = mmemcheck (nstrs, nvecs, fstr) t G allpopV allpopS  in
	let sn = itos n in
	let ifunname = mstrid i in
	let bexp = mexported i in
	let exp s = if bexp then [Mexport s] else [] in
	let rd = difference (mkset d) [ifunname] in
	(
	Mcom ("FUNSTART "@ifunname@" "@sn).
	Mdata .					-- FUN and info in data segment
	Malign .				-- on word boundary
	(if LinkWord then [Mword (const 0)] else [])@
	exp ifunname @				-- possible global label
    Mlabel ifunname .				-- local label
	(if n = 0 then
		-- Zero:ary function, i.e. global value definition.
		[
		Mword mvapgtag;			-- VAPG tag
		Mword (glob ("V"@ifunname));	-- function part
		Mword (const 0)			-- filler
		] @
	        hpwords vec1
	else
		[
		Mword (glob ("FUN"));		-- FUN tag
		Mword (glob ("V"@ifunname))	-- pointer to info vector
		]
	)
	@
		exp ("V"@ifunname) @
	(if CSymbols & bexp then let csym = "_HH_"@tail 2 ifunname in [Mexport csym; Mlabel csym] else []) @
	Mlabel ("V"@ifunname) .			-- Function info vector:
		Mword (const n).		-- arity
		Mword (glob (ifunname)).	-- function node pointer
		Mword (glob ("unw"@itosm MAXUNW n)).	-- unwind code
		Mword (glob ("vunw"@itosm MAXVUNW n)).	-- unwind of vector apply
		Mword (glob ("J"@sn@ifunname)).	-- function code
		Mword (glob ("S"@sn@ifunname)).	-- function call entry
		Mword (if oG ~= None then (glob ("Y"@sn@ifunname)) else (const 0)). -- stingy code
	        (if ProfileHeap then		-- 
		     Mword (idlit (v2l vec1))   --
		 else				-- 
		     Mword (const 0)		--
                )                            .  -- 
	        Mword (const 0) .		-- SPARE
		Mword (const 0) .		-- gc unmark chain
		Mword (const (length rd)).	-- # of refs
		map (Mword o glob) (filter canupd rd) @		-- refs
		Mtext .				-- text segment again
		exp ("S"@sn@ifunname) @		-- possible global call label
		exp ("J"@sn@ifunname) @		-- possible global jump label
		Malign .			-- align first instruction
	Mlabel ("S"@sn@ifunname).		-- local call label
		Mmove (Srel(n+1)) pushV.	-- adjust and push sp
	Mlabel ("J"@sn@ifunname).		-- local jump label
		Mfunbegin ifunname n.
		(if Profile     & ~id_isinst i then profcode   ("PC"@sn@ifunname) else []) @
		(if ProfileHeap & ~id_isinst i then samplecode ("PS"@sn@ifunname) else []) @
		(if CAFCall & n = 0 then [Mcall "entercaf"] else []) @
		(if ZapRedex & ~id_isinst i & (maytailcall G | maygc G)
                             then [Mmove (Sind n) (reg 0); 
				   Mmove (idlit "ZAP") (regind 0 0); 
				   Mmove (idlit ("V"@ifunname)) (regind 0 1)] @ 
		                   hpmoves vec2 (regind 0 3)
		             else []) @
                (if Trace & head 3 ifunname ~= "CVV" then [Mcall "do_enter"] else []) @
		c, [], [], [], 0)

-- IMPROVE THIS!!!
and
    maytailcall (FUNEND._)           = false
||  maytailcall (JFUN _._)           = true
||  maytailcall (UNWIND._)           = true
||  maytailcall (JGLOBAL _ _._)      = true
||  maytailcall (JMETHOD _ _._)      = true
||  maytailcall (EVAL _._)           = true
||  maytailcall (CALLFUN _ _._)      = true
||  maytailcall (CALLGLOBAL _ _ _._) = true
||  maytailcall (CALLMETHOD _ _ _._)   = true
||  maytailcall (_.gs)               = maytailcall gs
and maygc (FUNEND._) 		= false
||  maygc (MKAPLV _ _._) 	= true
||  maygc (CONSTR _ _ _ _._)	= true
||  maygc (RECBLOCK _._)	= true
||  maygc (ALLOC _._)		= true
||  maygc (BCONSTR _._)		= true
||  maygc (MKAP _._)		= true
||  maygc (MKCAP _._)		= true
||  maygc (CALLFUN _ _._)       = true
||  maygc (CALLGLOBAL _ _ _._)  = true
||  maygc (CALLMETHOD _ _ _._)  = true
||  maygc (_.gs)		= maygc gs

and /*canupd ('C'.'C'._) = false	-- CHAR constants
||  canupd ('C'.'I'._) = false  -- INT constants
||  canupd ('C'.'N'._) = false  -- TAG0 constants
||  */canupd _ = true

and
    samplecode ts =
	     [ Mcompare (glob "_sampleflag") (const 0) ;
               Mjcond eq ts			       ;
               Mcall "SAMPLE" 			       ;
               Mlabel ts 			       ]

and
-- Mcount makes a call to mcount (in runtime.M).
    profcode l =[
		 Mmove (idlit l) (reg 0);
		 Mcall "Mcount";
		 Mdata;
		 Mlabel l;
		 Mword (const 0);
		 Mtext;
		 Mcall "Mcountpost"]
and
    mfunend svf t g G V S =
	let (c, d, v, s, k) = M svf t g G V S
	in (Mcom "FUNEND".Mfunend.c, [], v, s, k)
and
    msfunstart svf t G i n =
	let (c, d, uv, us, k) = mmemcheck svf t G allpopV allpopS  in
	let ifunname = "Y"@itos n@mstrid i in
	((if mexported i then [Mexport ifunname; Mlabel ifunname]
	  else [Mlabel ifunname])@Mmove (Srel(n+1)) pushV.c, [], [], [], 0)
end
