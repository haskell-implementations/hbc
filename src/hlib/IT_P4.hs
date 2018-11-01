module I_Text_P4({-:"_#4"C:-}) where
data {-:"_#4"C:-} a b c d = {-:"P#4":-} a b c d deriving ()

instance  (Text a1, Text a2, Text a3, Text a4) => Text (a1, a2, a3, a4)  where
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

    showsPrec p (x1,x2,x3,x4) = 
	showChar '(' . shows x1 . showString ", " .
	               shows x2 . showString ", " .
	               shows x3 . showString ", " .
    	    	       shows x4 . showChar ')'

    showsType  ~(x1,x2,x3,x4) = 
	showChar '(' . showsType x1 . showString ", " .
	               showsType x2 . showString ", " .
	               showsType x3 . showString ", " .
    	    	       showsType x4 . showChar ')'
