module PreludeArray (array) where
import LML_array
#define lmlsarray {-:"Psarray":-}
import P_Array_data

{-# SPECIALIZE array :: (Int,Int) -> [Assoc Int a] -> Array Int a, ((Int,Int),(Int,Int)) -> [Assoc (Int,Int) a] -> Array (Int,Int) a #-}
array		:: (Ix a) => (a,a) -> [Assoc a b] -> Array a b
array b@(l,u) ivs = 
    MkArray b (lmlsarray 0 (index b u `max` -1) 0 [((if inRange b i then index b i else error "PreludeArray.array: index out of range"), v) | i := v <- ivs])
