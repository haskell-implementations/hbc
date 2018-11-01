module P_List_unwords  where
unwords			:: [String] -> String
unwords []		= ""
unwords [w] = w
unwords (w:ws) = w ++ ' ' : unwords ws
