module PreludeArray (elems) where
import LML_array
#define rawindex {-:"Prindexu":-}
import P_Array_data

{-# SPECIALIZE elems :: Array Int a -> [a] #-}
elems		:: (Ix a) => Array a b -> [b]
elems (MkArray b@(l,u) a) = fe a 0 ((index b u){-#STRICT#-})
				where fe :: LArray a -> Int -> Int -> [a]
				      fe a n m = if n > (m{-#NOEVAL#-}) then [] else ((rawindex a n){-#STRICT#-}) : fe a (n+1) m

