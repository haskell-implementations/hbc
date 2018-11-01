module P_Prelude_minInt(minInt) where
minInt		:: Int
#if defined(__alpha)
minInt          =  -9223372036854775807
#else
minInt		=  -2147483647
#endif
