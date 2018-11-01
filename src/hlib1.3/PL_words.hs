module PreludeX where
import Char(isSpace)
words			:: String -> [String]
words s			=  case dropWhile isSpace s of
				"" -> []
				s' -> w : words s''
				      where (w, s'') = break isSpace s'
-- SLOW
