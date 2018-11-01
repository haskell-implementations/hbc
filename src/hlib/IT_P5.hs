module I_Text_P5({-:"_#5"C:-}) where
#if 0
-- The poor compiler runs out of address registers when compiling the
-- nested list comprehensions here.
data {-:"_#5"C:-} a b c d e = {-:"P#5":-} a b c d e deriving ()
#else
data {-:"_#5"C:-} a b c d e = {-:"P#5":-} a b c d e deriving ()

instance  (Text a1, Text a2, Text a3, Text a4, Text a5) => Text (a1, a2, a3, a4, a5)  where
    readsPrec p = readParen False
    	    	    	    (\r0 -> [((x1,x2,x3,x4,x5), w) | 
					("(",s1) <- lex r0,
					(x1, r1) <- reads s1,
					(",",s2) <- lex r1,
					(x2, r2) <- reads s2,
					(",",s3) <- lex r2,
					(x3, r3) <- reads s3,
					(",",s4) <- lex r3,
					(x4, r4) <- reads s4,
					(",",s5) <- lex r4,
					(x5, r5) <- reads s5,
					(")",w)  <- lex r5 ] )

    showsPrec p (x1,x2,x3,x4,x5) = 
	showChar '(' . shows x1 . showString ", " .
	               shows x2 . showString ", " .
	               shows x3 . showString ", " .
	               shows x4 . showString ", " .
    	    	       shows x5 . showChar ')'

    showsType  ~(x1,x2,x3,x4,x5) = 
	showChar '(' . showsType x1 . showString ", " .
	               showsType x2 . showString ", " .
	               showsType x3 . showString ", " .
	               showsType x4 . showString ", " .
    	    	       showsType x5 . showChar ')'
#endif
