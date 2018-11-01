module P_List_takeWhile  where
takeWhile		:: (a -> Bool) -> [a] -> [a]
takeWhile p []		=  []
takeWhile p (x:xs) 
            | p x       =  x : takeWhile p xs
            | otherwise =  []
