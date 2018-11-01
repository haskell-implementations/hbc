module PreludeX where

instance  (Read a1, Read a2, Read a3) => Read (a1, a2, a3)  where
    readsPrec p = readParen False
    	    	    	    (\r0 -> [((x1,x2,x3), w) | 
					("(",s1) <- lex r0,
					(x1, r1) <- reads s1,
					(",",s2) <- lex r1,
					(x2, r2) <- reads s2,
					(",",s3) <- lex r2,
					(x3, r3) <- reads s3,
					(")",w)  <- lex r3 ] )
