module _LibArray where
import LML_array

data (Ix a) => Array a b = MkArray !(a,a) !(LArray b)

