module I_Text_Char() where
import P_T_util
instance  Text Char  where
    readsPrec p      = readParen False
    	    	    	    (\r -> [(c,t) | ('\'':s,t)<- lex r,
					    (c,_)     <- readLitChar s])

    showsPrec p '\'' = showString "'\\''"
    showsPrec p c    = showChar '\'' . showLitChar c . showChar '\''

    readList = readParen False (\r -> [(l,t) | ('"':s, t) <- lex r,
					       (l,_)      <- readl s	])
	       where readl ('"':s)	= [("",s)]
		     readl ('\\':'&':s)	= readl s
		     readl s		= [(c:cs,u) | (c ,t) <- readLitChar s,
						      (cs,u) <- readl t	      ]

    showList cs = showChar '"' . showl cs
		 where showl ""       = showChar '"'
		       showl ('"':cs) = showString "\\\"" . showl cs
		       showl (c:cs)   = showLitChar c . showl cs
    showsType x = showString "Char"

match			:: (Eq a) => [a] -> [a] -> ([a],[a])
match (x:xs) (y:ys) | x == y  =  match xs ys
match xs     ys		      =  (xs,ys)

asciiTab = -- Using an array drags in the array module.  listArray ('\NUL', ' ')
	   ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
	    "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI", 
	    "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
	    "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US", 
	    "SP"] 

readLitChar 		:: ReadS Char
readLitChar ('\\':s)	=  readEsc s
	where
	readEsc ('a':s)	 = [('\a',s)]
	readEsc ('b':s)	 = [('\b',s)]
	readEsc ('f':s)	 = [('\f',s)]
	readEsc ('n':s)	 = [('\n',s)]
	readEsc ('r':s)	 = [('\r',s)]
	readEsc ('t':s)	 = [('\t',s)]
	readEsc ('v':s)	 = [('\v',s)]
	readEsc ('\\':s) = [('\\',s)]
	readEsc ('"':s)	 = [('"',s)]
	readEsc ('\'':s) = [('\'',s)]
	readEsc ('^':c:s) | c >= '@' && c <= '_'
			 = [(chr (ord c - ord '@'), s)]
	readEsc s@(d:_) | isDigit d
			 = [(chr n, t) | (n,t) <- readDec s]
	readEsc ('o':s)  = [(chr n, t) | (n,t) <- readOct s]
	readEsc ('x':s)	 = [(chr n, t) | (n,t) <- readHex s]
	readEsc s@(c:_) | isUpper c
			 = let table = ('\DEL', "DEL") : zip ['\NUL'..] asciiTab
			   in case [(c,s') | (c, mne) <- table,
					     ([],s') <- [match mne s]]
			      of (pr:_) -> [pr]
				 []	-> []
	readEsc _	 = []
readLitChar (c:s)	=  [(c,s)]

showLitChar 		   :: Char -> ShowS
showLitChar c | c > '\DEL' && c < '\xa0' =  showChar '\\' . protectEsc isDigit (shows (ord c))
showLitChar '\DEL'	   =  showString "\\DEL"
showLitChar '\\'	   =  showString "\\\\"
showLitChar c | c >= ' '   =  showChar c
showLitChar '\a'	   =  showString "\\a"
showLitChar '\b'	   =  showString "\\b"
showLitChar '\f'	   =  showString "\\f"
showLitChar '\n'	   =  showString "\\n"
showLitChar '\r'	   =  showString "\\r"
showLitChar '\t'	   =  showString "\\t"
showLitChar '\v'	   =  showString "\\v"
showLitChar '\SO'	   =  protectEsc (== 'H') (showString "\\SO")
showLitChar c		   =  showString ('\\' : asciiTab!!ord c)

protectEsc p f		   = f . cont
			     where cont s@(c:_) | p c = "\\&" ++ s
				   cont s	      = s

