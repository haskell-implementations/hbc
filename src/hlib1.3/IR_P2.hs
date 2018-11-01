module PreludeX where

instance  (Read a, Read b) => Read (a,b)  where
    readsPrec p = readParen False
    	    	    	    (\r -> [((x,y), w) | ("(",s) <- lex r,
						 (x,t)   <- reads s,
						 (",",u) <- lex t,
						 (y,v)   <- reads u,
						 (")",w) <- lex v ] )
