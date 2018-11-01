module
#include "../Gcode/Gcodedef_t.t"
#include "mcodedef_t.t"
export	ccop, ccneg, ccrev, mopcc, mccop, dfcc, ccdf, sfcc, ccsf, isccdf, isccsf, invmop, negmop;
rec
    ccop EQ = (false, true, false)
 || ccop NE = (true, false, true)
 || ccop GT = (false, false, true)
 || ccop LT = (true, false, false)
 || ccop LE = (true, true, false)
 || ccop GE = (false, true, true)
and
    ccneg(x,y,z) = (~x, ~y, ~z)
and
    ccrev(x,y,z) = (z, y, x)
and
    mopcc(false, true, false) = eq
 || mopcc(true, false, true) = ne
 || mopcc(false, false, true) = gt
 || mopcc(true, false, false) = lt
 || mopcc(true, true, false) = le
 || mopcc(false, true, true) = ge
and
    mccop eq = (false, true, false)
 || mccop ne = (true, false, true)
 || mccop gt = (false, false, true)
 || mccop lt = (true, false, false)
 || mccop le = (true, true, false)
 || mccop ge = (false, true, true)
and
    dfcc eq = dfeq
||  dfcc ne = dfne
||  dfcc lt = dflt
||  dfcc le = dfle
||  dfcc gt = dfgt
||  dfcc ge = dfge
and
    ccdf dfeq = eq
||  ccdf dfne = ne
||  ccdf dflt = lt
||  ccdf dfle = le
||  ccdf dfgt = gt
||  ccdf dfge = ge
and
    sfcc eq = sfeq
||  sfcc ne = sfne
||  sfcc lt = sflt
||  sfcc le = sfle
||  sfcc gt = sfgt
||  sfcc ge = sfge
and
    ccsf sfeq = eq
||  ccsf sfne = ne
||  ccsf sflt = lt
||  ccsf sfle = le
||  ccsf sfgt = gt
||  ccsf sfge = ge
and
    isccdf dfeq = true
||  isccdf dfne = true
||  isccdf dflt = true
||  isccdf dfle = true
||  isccdf dfgt = true
||  isccdf dfge = true
||  isccdf _   = false
and
    isccsf sfeq = true
||  isccsf sfne = true
||  isccsf sflt = true
||  isccsf sfle = true
||  isccsf sfgt = true
||  isccsf sfge = true
||  isccsf _   = false
and invmop op =
	if isccdf op then
		dfcc (mopcc (ccrev (mccop (ccdf op))))
	else if isccsf op then
		sfcc (mopcc (ccrev (mccop (ccsf op))))
	else
		mopcc (ccrev (mccop op))
and negmop op =
	if isccdf op then
		dfcc (mopcc (ccneg (mccop (ccdf op))))
	else if isccsf op then
		sfcc (mopcc (ccneg (mccop (ccsf op))))
	else
		mopcc (ccneg (mccop op))
end
