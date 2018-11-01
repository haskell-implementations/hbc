module I_P5 where
#if 0
-- On some machines the poor compiler runs out of address registers when compiling the
-- nested list comprehensions here.
data {-:"_#5"C:-} a b c d e = {-:"P#5":-} a b c d e deriving ()
#else
data {-:"_#5"C:-} a b c d e = {-:"P#5":-} a b c d e deriving (Eq, Ord, Ix, Binary)
#endif
