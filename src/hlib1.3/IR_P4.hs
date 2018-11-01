module PreludeX where

instance  (Read a1, Read a2, Read a3, Read a4) => Read (a1, a2, a3, a4)  where
    readsPrec p = readParen False
    	    	    	    (\r0 -> [((x1,x2,x3,x4), w) | 
					("(",s1) <- lex r0,
					(x1, r1) <- reads s1,
					(",",s2) <- lex r1,
					(x2, r2) <- reads s2,
					(",",s3) <- lex r2,
					(x3, r3) <- reads s3,
					(",",s4) <- lex r3,
					(x4, r4) <- reads s4,
					(")",w)  <- lex r4 ] )
