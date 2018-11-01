module PreludeX where
{-# SPECIALIZE shows :: Int->String->String{-, Double->String->String, Float->String->String-} #-}	-- to get strictness info
shows 	    	:: (Show a) => a -> ShowS
shows x s	=  showsPrec 0 x s
