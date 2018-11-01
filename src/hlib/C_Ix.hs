module C_Ix(Ix(..)) where
class  (Ord a) => Ix a  where
    range		:: (a,a) -> [a]
    index		:: (a,a) -> a -> Int
    inRange		:: (a,a) -> a -> Bool
