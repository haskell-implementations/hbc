/*
**	integerfunc:	Some useful funcs.
**
*/
module
#include "Integer.t"
export _Int2Integer, _Integer2Int, _Integer2IntList, _IntegerDivMod, _Integer2String, _IntegerPowMod, _IntegerGcd, _IntegerSqrt, _IntegerAnd, _IntegerOr;
    _Int2Integer _x = PInt2Integer _x
and _Integer2Int _x = PInteger2Int _x
and _Integer2IntList _x = PInteger2IntList _x
and _IntegerDivMod _x _y = PIntegerDivMod _x _y
and _Integer2String _base _x = PInteger2String _base _x
and _IntegerPowMod _x _y _z = PIntegerPowMod _x _y _z
and _IntegerGcd _x _y = PIntegerGcd _x _y
and _IntegerAnd _x _y = PIntegerAnd _x _y
and _IntegerOr _x _y = PIntegerOr _x _y
and _IntegerSqrt _x = PIntegerSqrt _x
end
