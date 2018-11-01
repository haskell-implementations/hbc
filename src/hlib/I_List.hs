module I_List where
data {-:"PList"C:-} a = {-:"_[]":-} | a : [a] 	deriving (Eq, Ord, Binary)
-- The derived Ord is horrible, should use special one.
-- SLOW
