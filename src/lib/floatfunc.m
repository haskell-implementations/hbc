/*
**	floatfunc:	Some useful funcs.
**
*/
module
import Pitof: _Int -> _Double  {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import Pftoi: _Double -> _Int  {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};

export _ftoi, _ftos, _ftosf, _exp, _sin, _cos, _tan, _log, _sqrt, _asin,
       _acos, _atan, _atan2, _gamma, _sinh, _cosh, _tanh, _ceil, _floor, _fabs, _itof, _stof,
       _encodef, _decodef;

    _ftoi x = Pftoi x			-- convert Double to Int
and _itof x = Pitof x			-- convert Int to Double
and _ftos x = Dftos x			-- convert Double to String
and _ftosf x y z = Dftosf x y z		-- convert Double (z) to String, 
					-- where the string will be x characters, 
					-- with y digits following the decimal point.
and _exp x = Dexp x
and _sin x = Dsin x
and _cos x = Dcos x
and _tan x = Dtan x
and _log x = Dlog x
and _sqrt x = Dsqrt x
and _asin x = Dasin x
and _acos x = Dacos x
and _atan x = Datan x
and _atan2 x y = Datan2 x y
and _gamma x = Dgamma x
and _sinh x = Dsinh x
and _cosh x = Dcosh x
and _tanh x = Dtanh x
and _ceil x = Dceil x			-- return smallest integer r s.t. r > x
and _floor x = Dfloor x			-- return largest integer r s.t. r < x
and _fabs x = Dfabs x			-- absolute value of a Double
and _stof x = Dstof x			-- convert String to Double
and _encodef x y = Dencode x y
and _decodef x = Ddecode x
end
