module

#include "../misc/flags.t"
#include "../flic/flic.t"
#include "../flic/fconstr.t"
#include "../bwm/bwm.t"
#include "../expr/pprint.t"
#include "../expr/error.t"
#include "../expr/types_t.t"
#include "../expr/id.t"			/* prid */
#include "../expr/ttype.t"		/* prttype */
#include "../rename/renameutil.t"
#include "../rename/renenv.t"
#include "../transform/match.t"
#include "../transform/constr.t"
#include "../transform/lettrans.t"
#include "../transform/remsign.t"
#include "../transform/remclass.t"
#include "../transform/remlazy.t"
#include "../ExprE/Ecnv.t"
#include "../ExprE/Eprint.t"
#include "../ExprE/apconv.t"
#include "../ExprE/globalize.t"
#include "../ExprE/eqtrans.t"
#include "../ExprE/classtrans.t"
#include "../ExprE/addrestr.t"
#include "../ExprE/prexport.t"
#include "../ExprE/hprexport.t"
#include "../ExprE/unrec.t"
#include "../ExprE/predef.t"
#include "../ExprE/forcearity.t"
#include "../ExprE/addspec.t"
#include "../ExprE/remunused.t"
#include "../type/tchk.t"
#include "../type/prefix.t"
#include "../etype/cunrec.t"
#include "../etype/tchk.t"
#include "../etype/substear.t"
#include "../etype/xsubst.t"    /* prsubst */
#include "../llift/addclose.t"
#include "../llift/llift.t"
#include "../llift/Bconv.t"
#include "../llift/addarity.t"
#include "../llift/chkind.t"
#include "../Gcode/Gcodedef_t.t"
#include "../Gcode/Gmain.t"
#include "../Gcode/Gprint.t"
#include "../Gopt/gopt.t"
#include "../Gopt/goptgm.t"
#include "../mcode/mcode.t"
#include "../mcode/mprint.t"
#include "../mopt/mopt.t"
#include "../strict/strict.t"
#include "../strict/mkstrict.t"
#include "../strict/strictcon.t"
#include "../strict/calcstrict.t"
#include "../simpl/simpl.t"
#include "../simpl/flow.t"
#include "../mcode/machine.tt"
#include "../zf/remzf.t"
#include "../misc/ioc.t"
#include "../ilml/imain.t"
#include "topexprs.t"
#include "files.t"
#include "version.t"


#define MAXERRCHAR 4000

main = if Interactive then imain else interact (\input.

let display t e = t @ "\n" @ ppr e @ "\n" in
let Edisplay t e = t @ "\n" @ pr e @ "\n" in
let fp f t = if f then t else "" in
let rec getemsg [] = []
||      getemsg ([].r) = getemsg r
||      getemsg (a._) = head MAXERRCHAR (concmap (\e.e@"\n") a)
in
let stopit emsg = fstderr@"Errors:\n"@emsg@"\nCompilation aborted\n"@failexit 1 in
let prpragmas ps = "Pragmas:\n" @ mix (map prpragma (concmap snd ps)) "\n" in

let u1 = unum1
and e0 = expr0
and e02= expr1
and e1 = expr2
in

let (e1s, dflts) = remsign def_dflts e1 in
let (e1q, u2) = remzf e1s u1 in
let e1c = remclass e1q in
let (e2, u3) = constrtr e1c u2 in
let (e2z, u4) = remlazy e2 u3 in

-- Conctype stuff.
let e2u = cunrec e2z  in      
let (concEs,terr0,S0,e2t,npre0,u4a) = etcheck errmap e2u u4 in
let e2e = substear concEs e2t in
let (e2r, u4r) = if ConcType then (e2e, u4a) else (e2z, u4) in

let (e3, u5) = remmatch e2r u4r in
let e4 = remdeep e3 in
let E01 = Ecnv e4 in
let E02 = if NoUnrec then E01 else unrec E01 in
let (E020, u50) = addspec pragmas E02 u5 in
let (E03, u51) = addspecrestr E020 u50 in
let (E0, u6) = addrestr (map fst pragmas) E03 u51 in
let (terr1, S1, E1_1, npre1, u71) = tcheck errmap E0 dflts u6 in
let (terr, S, E1, npre, u7) = if ConcType then (terr0, S0, E0, npre0, u6) else (terr1, S1, E1_1, npre1, u71) in
let E1a = globalize E1 in
let E11a = apconv E1a in
let E11 = remunused E11a in
let E12 = classtrans E11 in
let E2 = eqtrans S E12 in
let (E2y, u7y) = etaexpand E2 u7 in
let (E2p, u70) = eforcearity E2y u7y in
let E2x = predef E2p in
let (E2l, u71) = lambdalift1 E2x u70 in
let (u8, E21) = if NoSimpl then (u71, E2l) else Esimpl u71 E2l in
let strinfo = calcstrict E21 in
let E31 = if NoStrictAnal then E21 else strictanal E21 strinfo in
let E32 = mkstrict E31 in	-- make it strict if Strict flag is on
let E3 = strictcon E32 in
let E4 = addclose E3 in
let (E51,u9) = lambdalift E4 u8 in
let (E5,u10) = if NoBConv then (E51,u9) else Bconv E51 u9 in
let (hexp, hexperr) = case hprexport allinsts preenv E5 strinfo in Yes s : (s, []) || No s : ("", [s]) end in
let lexp = prexport E5 in
let (exp, oexp, experr) = if Curry then (hexp, lexp, hexperr) else (lexp, hexp, []) in
let E6 = if NoChkInd then addarity E5 [] else achkind (addarity E5 []) in
let Et = E6 in
let gco0 = Gmain Et in
let gco = if NoGOpt then gco0 else gopt gco0 in
let mco0 = mmain gco in
let mco = if NoMOpt then mco0 else mopt mco0 in
let ass = assemblercode mco in
--let (EF2,u11) = lambdalift E51 u10 in
let (EF1,u11) = fconstr E4 u10 in
conc [
fstderr								;
(if Verbose then (if Curry then "Haskell B. " @ if H1_3 then "1.4" else "1.2" else "LML") @ " " @ version @ ".\n" else "")			;
fstdout								;
fp PrErrmap     (concmap (\(f, (n, l))."\""@n@"\", line "@itos l@", def: "@f@"\n") (reverse errmap)) ;
fp PrInput 	(display "---Input:"    e0) 			;
fp PrCurry 	(display "---Curry:"    e02) 			;
fp PrRename 	(display "---Renamed:"  e1 @ prpragmas pragmas @ envinfo preenv);
fp PrRemSign	(display "---Remsign:"  e1s)			;
--fp PrGenderiv	(display "---Genderiv:" e1g)			;
fp PrRemzf	(display "---Remzf:"    e1q)			;
fp PrRemClass	(display "---Remclass:" e1c)			;
fp PrConstr	(display "---Constr:"   e2) 			;
fp PrRemLazy	(display "---Remlazy:"	e2z)			;
fp (PrCunrec&ConcType)	   
		(display "---Cunrec:"	e2u)			;
fp (PrSubst&ConcType)      ("---Substitution:\n"@ prsubst S0@"\n")	;
fp (PrAppEarley&ConcType)  ("---Applyear:\n"@concmap ppr concEs)      ;
fp (PrSubEarley&ConcType)	(display "---Substear:" e2e)		;
fp PrRemmatch	(display "---Remmatch:" e3) 			;
fp PrRemdeep	(display "---Remdeep:"  e4) 			;
let emsg = getemsg (map (errors errmap) [e1; e1s; e1c; e2; e3]) in
if emsg ~= [] then
        stopit emsg
else (conc [
	fp PrEcnv	(Edisplay "---Ecnv:"    E01)		;
	fp PrAddfrom	(Edisplay "---Unrec:" E02)		;
	fp PrAddrestr	(Edisplay "---addrestr:"    E0)		;
	fp PrType	("---Type:\n" @ prpre npre @"\n"@pr E1) ;
	fp ShowTopTypes (prettypre (getgp npre))	        ;

let emsg = getemsg [terr] in
if emsg ~= [] then
        stopit emsg
else (conc [
	fp PrRemdeep	(Edisplay "---globalize:"  E1a)		;
	fp PrApconv	(Edisplay "---apconv:"  E11)		;
	fp PrClasstrans	(Edisplay "---classtrans:"  E12)	;
	fp PrEqtrans	(Edisplay "---eqtrans:"  E2)		;
	fp PrSimpl	(Edisplay "---forcearity&etaexpand:"  E2p)	;
	fp PrPredef	(Edisplay "---predef:"  E2x)		;
	fp PrLambdalift	(Edisplay "---lambdalift:" E2l)		;
	fp PrSimpl	(Edisplay "---simpl:"  E21)		;
	fp PrOutFlic	(Etofprint  EF1)			;
        fp PrOutBwm     (fbwm@bwm u8 E6@fstdout)                ;
        fp PrStrictinfo ("---strictness info:\n"@prstrict strinfo@"\n");
	fp PrStrict	(Edisplay "---strict:"  E3)		;
	fp PrAddclose	(Edisplay "---addclose:" E4)		;
	fp PrLambdalift	(Edisplay "---lambdalift:" E51)		;
	fp PrBconv	(Edisplay "---Bconv:" E5)		;
	fp PrAddarity	(Edisplay "---addarity:" E6)		;
        fp PrTransformed(Edisplay "---Transformed program:" Et) ;
        fp PrInput	("Unique: "@show_list show_int [startu;u1;u2;u3;u4;u5;u50;u51;u6;u7;u7y;u70;u71;u8;u9;u10]@"\n")		;
	fp PrGcodeUnopt	("---Gcode unopt:\n" @ (Gprints gco0))	;
	fp PrGcode	("---Gcode:\n" @ (Gprints gco))		;
	fp Gflag	(fgcode@Gprints (goptgm gco)@fstdout)	;
	fp PrMcode	("---Mcode:\n" @ (mprint mco))		;
if experr ~= [] then
        stopit (getemsg (map (errors errmap o mkerror) experr))
else (conc [
	ftype							;
	fp Type		exp					;
	(if Type & BothTypes then fotype @ oexp else "")	;
	fasm							;
	fp Code		ass
])
])
])
]
)
end
