module PreludeX where
{-# SPECIALIZE notElem :: Int -> [Int] -> Bool, Char -> [Char] -> Bool, String -> [String] -> Bool #-}
notElem		:: (Eq a) => a -> [a] -> Bool
notElem	x []	=  True
notElem x (y:ys)=  x /= y && notElem x ys
