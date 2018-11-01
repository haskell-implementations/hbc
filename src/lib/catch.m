/*
**	catch:		catch an error produced by throw.  The first argument
**			is evaluated and returned if it does not evaluate
**			to an error.  If it is the second argument is given
**			the error message as an argument ant that is returned.
*/
module
import Pcatch:*a->(_List _Char->*a)->*a;
export _catch;
_catch _a _f = Pcatch _a _f
end
