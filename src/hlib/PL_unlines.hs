module P_List_unlines  where
unlines			:: [String] -> String
unlines [] = []
unlines (l:ls) = l ++ '\n' : unlines ls
