module PreludeX where
unlines			:: [String] -> String
unlines [] = []
unlines (l:ls) = l ++ '\n' : unlines ls
