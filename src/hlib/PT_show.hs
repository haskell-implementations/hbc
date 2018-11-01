module	P_Text_show(show) where
{-# SPECIALIZE show :: Int->String{-, Double->String, Float->String-} #-}	-- to get strictness info (where it pays)
show 	    	:: (Text a) => a -> String
show x 	    	=  shows x ""
