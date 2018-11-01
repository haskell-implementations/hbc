module I_RealFloat_Float where
#include "../lib/flt.h"
import LML_Double

instance  RealFloat Float where
	floatRadix _	= FLT_RADIX
	floatDigits _	= FLT_DIGITS
	floatRange _	= (FLT_MINEXP, FLT_MAXEXP)
	decodeFloat x 	= {-:"Dfdecode":-} x
	encodeFloat x y	= {-:"Dfencode":-} x y

