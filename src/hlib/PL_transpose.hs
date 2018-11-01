module P_List_transpose  where
transpose		:: [[a]] -> [[a]]
transpose		=  foldr 
			     (\xs xss -> zipWith (:) xs (xss ++ repeat []))
			     []
-- SLOW
