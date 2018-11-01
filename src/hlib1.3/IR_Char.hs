module PreludeX where
import Char__readLitChar
instance  Read Char  where
    readsPrec p      = readParen False
    	    	    	    (\r -> [(c,t) | ('\'':s,t)<- lex r,
					    (c,_)     <- readLitChar s])

    readList = readParen False (\r -> [(l,t) | ('"':s, t) <- lex r,
					       (l,_)      <- readl s	])
	       where readl ('"':s)	= [("",s)]
		     readl ('\\':'&':s)	= readl s
		     readl s		= [(c:cs,u) | (c ,t) <- readLitChar s,
						      (cs,u) <- readl t	      ]

