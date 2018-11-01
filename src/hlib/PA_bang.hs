module PreludeArray ((!)) where
import LML_array
#define lmlindex {-:"Paindex":-}
import P_Array_data

{-# SPECIALIZE (!) :: Array Int b -> Int -> b, Array (Int,Int) b -> (Int,Int) -> b #-}
(!)		:: (Ix a) => Array a b -> a -> b
MkArray b a ! i = if inRange b i then lmlindex a (index b i) else error "PreludeArray.(!): index out of range"

