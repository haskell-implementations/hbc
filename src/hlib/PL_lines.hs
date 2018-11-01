module P_List_lines  where
lines			:: String -> [String]
lines ""		=  []
lines s                 =  let (l, s') = break (== '\n') s
                           in  l : case s' of
                                        []      -> []
                                        (_:s'') -> lines s''
-- SLOW
