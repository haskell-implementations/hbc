module PreludeX where
data [] a = [] | a : [a] 	deriving (Eq, Ord)
-- The derived Ord is horrible, should use special one.
-- SLOW
