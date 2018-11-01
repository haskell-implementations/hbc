module PreludeX where
instance Bounded Int where
    minBound
#if defined(__alpha)
        =  -9223372036854775807
#else
	=  -2147483647
#endif
    maxBound
#if defined(__alpha)
        =  9223372036854775807
#else
	=  2147483647
#endif

