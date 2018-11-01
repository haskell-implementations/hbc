module  PreludeArray (amap) where
import LML_array
import P_Array_data

amap :: (Ix a) => (b -> c) -> Array a b -> Array a c
amap f a = array b [i := f (a!i) | i <- range b]
	where b = bounds a
