module Numeric__showSigned where

showSigned:: (Num a, Ord a) => (a -> ShowS) -> Int -> a -> ShowS
showSigned showPos p x = if x < 0 then showParen (p > 6)
						 (showChar '-' . showPos (-x))
				  else showPos x

