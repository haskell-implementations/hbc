module PreludeX where
{-# SPECIALIZE show :: Int->String{-, Double->String, Float->String-} #-}	-- to get strictness info (where it pays)
show 	    	:: (Show a) => a -> String
show x 	    	=  shows x ""
