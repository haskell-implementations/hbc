module Char__showLitChar(showLitChar) where

import Char

asciiTab = -- Using an array drags in the array module.  listArray ('\NUL', ' ')
	   ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
	    "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI", 
	    "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
	    "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US", 
	    "SP"] 

showLitChar 		   :: Char -> ShowS
showLitChar c | c > '\DEL' && c < '\xa0' =  showChar '\\' . protectEsc isDigit (shows (fromEnum c))
showLitChar '\DEL'	   =  showString "\\DEL"
showLitChar '\\'	   =  showString "\\\\"
showLitChar c | c >= ' '   =  
	if c <= '\xff' then
	    showChar c
	else
	    -- Unicode
	    showString ("\\u" ++ showHex 4 (fromEnum c))
showLitChar '\a'	   =  showString "\\a"
showLitChar '\b'	   =  showString "\\b"
showLitChar '\f'	   =  showString "\\f"
showLitChar '\n'	   =  showString "\\n"
showLitChar '\r'	   =  showString "\\r"
showLitChar '\t'	   =  showString "\\t"
showLitChar '\v'	   =  showString "\\v"
showLitChar '\SO'	   =  protectEsc (== 'H') (showString "\\SO")
showLitChar c		   =  showString ('\\' : asciiTab!!fromEnum c)

protectEsc p f		   = f . cont
			     where cont s@(c:_) | p c = "\\&" ++ s
				   cont s	      = s

showHex :: Int -> Int -> String
showHex 0 _ = ""
showHex n i = showHex (n-1) (i `div` 16) ++ ["0123456789abcdef" !! (i `mod` 16)]

