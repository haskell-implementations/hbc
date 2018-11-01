module P_Prelude_maxInt(maxInt) where
maxInt		:: Int
#if defined(__alpha)
maxInt          =  9223372036854775807
#else
maxInt		=  2147483647
#endif
