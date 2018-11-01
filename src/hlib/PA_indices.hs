module PreludeArray (indices) where
import LML_array
import P_Array_data

indices		:: (Ix a) => Array a b -> [a]
indices (MkArray b _) = range b
