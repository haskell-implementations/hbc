module I_Text_P2({-:"_#2"C:-}) where
data {-:"_#2"C:-} a b = {-:"P#2":-} a b deriving ()

instance  (Text a, Text b) => Text (a,b)  where
    readsPrec p = readParen False
    	    	    	    (\r -> [((x,y), w) | ("(",s) <- lex r,
						 (x,t)   <- reads s,
						 (",",u) <- lex t,
						 (y,v)   <- reads u,
						 (")",w) <- lex v ] )

    showsPrec p (x,y) = showChar '(' . shows x . showString ", " .
    	    	    	    	       shows y . showChar ')'
    showsType  ~(x,y) = showChar '(' . showsType x . showString ", " .
    	    	    	    	       showsType y . showChar ')'
