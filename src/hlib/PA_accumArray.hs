module  PreludeArray (accumArray) where
import LML_array
#define lmlarray {-:"Parray":-}
import P_Array_data

accumArray :: (Ix a) => (b -> c -> b) -> b -> (a,a) -> [Assoc a c] -> Array a b
accumArray f z b@(l,u) ivs  = 
	MkArray b (lmlarray 0 (index b u `max` -1) (foldl f z) [((if inRange b i then index b i else error "PreludeArray.accumArray: index out of range"), v) | i := v <- ivs])

