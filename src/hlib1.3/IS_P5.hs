module PreludeX where

instance  (Show a1, Show a2, Show a3, Show a4, Show a5) => Show (a1, a2, a3, a4, a5)  where
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
