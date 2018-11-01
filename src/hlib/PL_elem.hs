module P_List_elem  where
--infix  4 `elem`
{-# SPECIALIZE elem :: Int -> [Int] -> Bool, Char -> [Char] -> Bool, String -> [String] -> Bool #-}
elem		:: (Eq a) => a -> [a] -> Bool
elem _ []	= False
elem x (y:ys)	= x==y || elem x ys
