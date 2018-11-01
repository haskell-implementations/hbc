module PreludeX where
lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup x' ((x,y):xys) = if x == x' then Just y else lookup x' xys
