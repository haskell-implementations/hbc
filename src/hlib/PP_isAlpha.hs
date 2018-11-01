module P_Prelude_ctype where
-- Modified to handle ISO 8859-1
isISO c                 =  ord c >= 0 && ord c <= 255
isAscii c	 	=  ord c >= 0 && ord c < 128
isControl c		=  c < ' ' || c >= '\DEL' && c <= '\x9f'
isPrint c		=  c >= ' ' && c <= '~' || c >= '\xa0' && c <= 'ÿ'
isSpace c		=  c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v' || c == '\xa0'
isUpper c		=  c >= 'A' && c <= 'Z' || c >= 'À' && c <= 'Ö' || c >= 'Ø' && c <= 'Þ'
isLower c		=  c >= 'a' && c <= 'z' || c >= 'ß' && c <= 'ö' || c >= 'ø' && c <= 'ÿ'
isAlpha c		=  isUpper c || isLower c
isDigit c		=  c >= '0' && c <= '9'
isAlphanum c		=  isAlpha c || isDigit c

-- These almost work for ISO-Latin-1 (except for ß <-> ÿ)
toUpper, toLower	:: Char -> Char
toUpper c | isLower c	=  chr (ord c - ord 'a' + ord 'A')
	  | otherwise	=  c

toLower c | isUpper c	=  chr (ord c - ord 'A' + ord 'a')
	  | otherwise	=  c
