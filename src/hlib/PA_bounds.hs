module PreludeArray (bounds) where
import LML_array
import P_Array_data

{-# SPECIALIZE bounds :: Array Int a -> (Int,Int) #-}
bounds		:: (Ix a) => Array a b -> (a,a)
bounds (MkArray b _)  = b
