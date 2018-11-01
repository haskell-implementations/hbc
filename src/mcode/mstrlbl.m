module
#include "../Gcode/Gcodedef_t.t"
export mstrlbl, tmplbl;
rec
/* mstrlbl converts a Gcode label, as appearing in a LABEL instruction,
 * into a string to be used as a label in the mcode.
 */
    mstrlbl (Label i) = 'L'.'L'.itos i
||  mstrlbl (Notalabel) = "Nolabel" -- fail "fail mstrlbl: Notalabel\n"

and tmplbl i = 'L'.'T'.itos i
end
