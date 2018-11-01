module I_Text_P3({-:"_#3"C:-}) where
data {-:"_#3"C:-} a b c = {-:"P#3":-} a b c deriving ()

instance  (Text a1, Text a2, Text a3) => Text (a1, a2, a3)  where
    readsPrec p = readParen False
    	    	    	    (\r0 -> [((x1,x2,x3), w) | 
					("(",s1) <- lex r0,
					(x1, r1) <- reads s1,
					(",",s2) <- lex r1,
					(x2, r2) <- reads s2,
					(",",s3) <- lex r2,
					(x3, r3) <- reads s3,
					(")",w)  <- lex r3 ] )

    showsPrec p (x1,x2,x3) = 
	showChar '(' . shows x1 . showString ", " .
	               shows x2 . showString ", " .
    	    	       shows x3 . showChar ')'
    showsType  ~(x1,x2,x3) = 
	showChar '(' . showsType x1 . showString ", " .
	               showsType x2 . showString ", " .
    	    	       showsType x3 . showChar ')'
