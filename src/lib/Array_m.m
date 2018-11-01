/*
**	Array_m:	LML-interface to lower level routines
**
*/
module
#include "ArrayM.t"
export _array, _sarray,
       _lowerbound,
       _upperbound,
       Pfail_funny_bounds,
       Pfail_index_too_small,
       Pfail_index_too_big,
       Pfail_undefined_elem,
       Pfail_multiple_index,
       Pfail_out_of_range;

    _array _x _y _z _w = Parray _x _y _z _w
and _sarray _x _y _z _w = Psarray _x _y _z _w
and _lowerbound _a = Plowerbound _a
and _upperbound _a = Pupperbound _a

and	Pfail_funny_bounds _lo _hi =
		Pfail ("assocarray: funny bounds: lo=" @ _itos _lo @ 
		      ", hi=" @ _itos _hi @ "\n")
and	Pfail_index_too_small _siz _a _i = 
		Pfail( 	"arrayindexing: index = " @ _itos _i @ 
		  	" too small (" @ _itos _a @ "..." @ _itos (_a+_siz-1) @ ")\n"
			)
and	Pfail_index_too_big _siz _a _i = 
		Pfail( 	"arrayindexing: index = " @ _itos _i @ 
		  	" too big (" @ _itos _a @ "..." @ _itos (_a+_siz-1) @ ")\n"
			)
and     Pfail_undefined_elem = Pfail "Undefined array element"
and     Pfail_multiple_index = Pfail "Multiply defined array element"
and     Pfail_out_of_range   = Pfail "Out-of-range array association"
end
