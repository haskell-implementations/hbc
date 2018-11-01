module  PreludeArray (ixmap) where
import LML_array
import P_Array_data

ixmap :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c -> Array a c
ixmap b f a = array b [i := a ! f i | i <- range b]
