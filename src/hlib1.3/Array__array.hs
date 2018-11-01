module Array__array where

import Ix
import LML_array
import _LibArray

#define lmlsarray {-:"Psarray":-}

{-# SPECIALIZE array :: (Int,Int) -> [(Int, a)] -> Array Int a, ((Int,Int),(Int,Int)) -> [((Int,Int), a)] -> Array (Int,Int) a #-}
array		:: (Ix a) => (a,a) -> [(a, b)] -> Array a b
array b@(l,u) ivs = 
    MkArray b (lmlsarray 0 (index b u `max` -1) 0 [((if inRange b i then index b i else error "Array.array: index out of range"), v) | (i, v) <- ivs])

