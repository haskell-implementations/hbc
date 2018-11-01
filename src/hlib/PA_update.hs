module  PreludeArray ((//)) where
import LML_array
import P_Array_data

{-# SPECIALIZE (//) :: Array Int b -> [Assoc Int b] -> Array Int b #-}
(//) :: (Ix a) => Array a b -> [Assoc a b] -> Array a b
a // us	= array (bounds a)
		([i := a!i | i <- indices a \\ [i | i:=_ <- us]] ++ us)
