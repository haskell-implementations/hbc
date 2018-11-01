#define lmlindex {-:"Paindex":-}

module Array__bang where
import Ix
import LML_array
import _LibArray

infixl 9 !

{-# SPECIALIZE (!) :: Array Int b -> Int -> b, Array (Int,Int) b -> (Int,Int) -> b #-}
(!)		:: (Ix a) => Array a b -> a -> b
MkArray b a ! i = 
	if inRange b i then lmlindex a (index b i) 
	else error ("Array.(!): index out of range (ix=" ++ show (index b i) ++ "bound=" ++ show(index b (snd b)))

