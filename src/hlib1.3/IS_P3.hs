module PreludeX where

instance  (Show a1, Show a2, Show a3) => Show (a1, a2, a3)  where
    showsPrec p (x1,x2,x3) = 
	showChar '(' . shows x1 . showString ", " .
	               shows x2 . showString ", " .
    	    	       shows x3 . showChar ')'
    showsType  ~(x1,x2,x3) = 
	showChar '(' . showsType x1 . showString ", " .
	               showsType x2 . showString ", " .
    	    	       showsType x3 . showChar ')'
