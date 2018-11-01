/*
**	throw:		produce an error (almost bottom), with an
**			error message attached.  This is almost like fail
**			but it can be caught by catch.
*/
module
import Pthrow:(_List _Char)->*a;
export _throw;
_throw _s = Pthrow _s
end
