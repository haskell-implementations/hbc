module PreludeArray (listArray) where
import LML_array
#define lmlsarray {-:"Psarray":-}
import P_Array_data

{-# SPECIALIZE listArray :: (Int,Int) -> [a] -> Array Int a #-}
listArray	:: (Ix a) => (a,a) -> [b] -> Array a b
listArray b@(l,u) vs = 
	let hi = index b u
	    fla :: Int -> Int -> [a] -> [(Int,a)]
	    fla _ _ [] = []
	    fla i m (x:xs) = if i>m then [] else (i,x) : fla (i+1) m xs
	in  MkArray b (lmlsarray 0 (hi `max` -1) 0 (fla 0 hi vs))

-- !!! is this correct?
