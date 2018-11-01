module PreludeX where

instance  (Show a, Show b) => Show (a,b)  where
    showsPrec p (x,y) = showChar '(' . shows x . showString ", " .
    	    	    	    	       shows y . showChar ')'
    showsType  ~(x,y) = showChar '(' . showsType x . showString ", " .
    	    	    	    	       showsType y . showChar ')'
