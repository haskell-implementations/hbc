module
#include "../expr/id.t"
#include "../misc/util.t"
export nilenv, lookenv, addenv, listenv;
local
    type Genv = Env (List(Id#Int))
in
rec
    nilenv = Env []
and lookenv (Env l) i = assocdefeq eqid i l 0
and addenv (Env l) idl std =
	Env(reverse(combine(idl, from(std+1))) @ l), std + length idl
and listenv (Env l) n = 
	let rec f pos ((i,posi).l) =
		if pos < posi then
			dummyid . f (pos+1) ((i,posi).l)
		else
		if pos = posi then
			i . f (pos+1) l
		else
			fail "listenv"
	    || f pos [] = if pos <= n then dummyid.f (pos+1) [] else []
	in
		f 1 (reverse l)
end
end
