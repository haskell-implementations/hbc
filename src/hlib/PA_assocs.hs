module  PreludeArray (assocs) where
import LML_array
import P_Array_data

assocs :: (Ix a) => Array a b -> [Assoc a b]
assocs a = [i := a!i | i <- indices a]

