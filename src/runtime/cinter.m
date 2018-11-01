--
-- Print M-code to glue a C function to LML.
--
#include "../mcode/limit.h"

let lines ls = concmap (\l.mix l "\t"@"\n") ls
and defInclude = "../runtime/machdep.M"
in
let (sep, CAFCall, argv') = case argv in
			       (('-'.fs).argv): ((if mem 'Z' fs then "" else "_"),
						 mem 'C' fs, argv)
			    || _ : ("",false,argv)
			    end
in
let geninc incFile =
    lines [
	   ["#include \""@incFile@"\""]
	  ]
in
let generate sep lmlName arity cName doforce isdata =
    let cname  = "C"@sep@lmlName
    and vcname = "VC"@sep@lmlName
    and narity = stoi arity in
    let jname  = "J"@arity@cname
    and sname  = "S"@arity@cname
    and uarity = if narity < MAXUNW  then arity else "N"
    and varity = if narity < MAXVUNW then arity else "N"
    in
    lines ([
	   [";;"];
	   [";; Interface (from "@progname@") for calling C function '"@cName@"' from LML as '"@lmlName@"'."];
	   [";;"];
	   ["";".data"];
	   ["";".malign"];
	   ["";".export";cname];
	   ["";".word";"0"];		-- only for VAP ?
	   [cname@":"]
	  ]@
          (if narity > 0 then
	       [
		["";".word";"FUN"];
		["";".word";vcname]
	       ]
	   else
	       [
		["";".word";"VAPG"]; -- NR 921026
		["";".word";vcname];
		["";".word";"0"]
	       ]
	  )@
          [
	   ["";".export";vcname];
	   [vcname@":"];
	   ["";".word";"$"@arity];
	   ["";".word";cname];
	   ["";".word";"unw"@uarity];
	   ["";".word";"vunw"@varity];
	   ["";".word";jname];
	   ["";".word";sname];
	   ["";".word";"$0"];	-- stingy code
	   ["";".word";"$0"];	-- heap profile info
	   ["";".word";"$0"];	-- spare
	   ["";".word";"$0"];	-- unmark chain
	   ["";".word";"$0"];	-- nref
	   ["";".text"];
	   ["";".export";sname];
	   ["";".export";jname];
	   ["";".malign"];
	   [sname@":"];
	   ["";"move";"$"@itos(narity+1)@"(Sp),Vpush"];
	   [jname@":"];
	   ["";".funbegin"; lmlName; itos narity]] @
	   (if CAFCall & narity = 0 then [["";"call";"entercaf"]] else [])
           @
	   (if doforce then
	       conc (for 0 (narity-1) (\n.[["";"move";itos n@"(Sp),r0"]; ["";"move";"0(r0),INDREG"]; ["";"call";"oforce(INDREG)"]]))
           else
               []) @
          [
	   ["";"comp";"Hp,_ehp"];
	   ["";"jlth";"GC"@lmlName];
	   ["";"call";"GARB"];
	   ["GC"@lmlName@":"]
          ] @
	   conc (for 0 (narity-1) (\k.let n = narity-1-k in [["";"move";itos n@"(Sp),r0"]; ["";"CPUSHARG"@itos n@"(r0)"]])) @
          [
	   ["";"move";"Sp,_ep"];
	   ["";"move";"Hp,_hp"];
	   ["";"CCALL("@arity@",_"@cName@")"];
	   ["";"move";"_hp,Hp"];
	   ["";"move";"_ep,Sp"]
          ] @
           (if ~isdata then
               [
	        ["";"move";"CRETR,r0"];
        	["";"move";"$"@arity@"(Sp),Sp"];
	        ["";"jumpf";"evalupdunw"]
               ]
            else
               [
	        ["";"move";"CRETR,r1"];
		["";"move";"Vpop,Sp"];
		["";"move";"-1(Sp),r0"];
		["";"move";"0(r1),0(r0)"];
		["";"move";"1(r1),1(r0)"];
		["";"move";"2(r1),2(r0)"];
		["#ifdef HPROFILE"];
		["";"move";"3(r1),3(r0)"];
		["#ifdef SLOP2"];
		["";"move";"4(r1),4(r0)"];
		["#endif"];
		["#endif"];
		["";"return"]
               ]) @
	   [["";".funend"]
	  ])
in
let generate4 s l =
    case choplist (splitat ' ') l in
	[l; a; c] & (all isdigit a) : generate s l a c true false
    ||  [l; a; c; f; d] & (all isdigit a) : generate s l a c (f="T") (d="T")
    ||  _			    : fail ("Bad input: "@l)
    end
in
let getflags ('-'.fs) = fs
||  getflags _ = ""
in
case argv' in
    [lmlName; arity; cName; incFile] & (all isdigit arity) : geninc incFile    @ generate sep lmlName arity cName true false
||  [lmlName; arity; cName]          & (all isdigit arity) : geninc defInclude @ generate sep lmlName arity cName true false
||  ["-"]                                                  : geninc defInclude @ concmap (generate4 sep) (choplist (splitat '\n') input)
||  _ : "Usage: "@progname@" [-ZC] lml-name arity C-name include-file\n"
end
