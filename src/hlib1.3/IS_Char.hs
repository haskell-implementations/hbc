module PreludeX where
import Char__showLitChar
instance  Show Char  where
    showsPrec p '\'' = showString "'\\''"
    showsPrec p c    = showChar '\'' . showLitChar c . showChar '\''

    showList cs = showChar '"' . showl cs
		 where showl ""       = showChar '"'
		       showl ('"':cs) = showString "\\\"" . showl cs
		       showl (c:cs)   = showLitChar c . showl cs
    showsType x = showString "Char"

