module  PreludeArray (accum) where
import LML_array
import P_Array_data

accum :: (Ix a) => (b -> c -> b) -> Array a b -> [Assoc a c] -> Array a b
-- SLOW
accum f a l = foldl (\a (i := v) -> a // [i := f (a!i) v]) a l
