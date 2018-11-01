module PreludeX where
unzip			:: [(a,b)] -> ([a],[b])
unzip			=  foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])
--SLOW
