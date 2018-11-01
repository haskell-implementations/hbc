module PreludeX where
unwords			:: [String] -> String
unwords []		= ""
unwords [w] = w
unwords (w:ws) = w ++ ' ' : unwords ws
