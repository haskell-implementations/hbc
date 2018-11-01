module Numeric__readFloat(readFloat) where
import Ratio
import Char
import Numeric__readInt(readDec, nonnull)

readFloat:: (RealFloat a) => ReadS a
readFloat r = [(fromRational ((n%1)*10^^(k-d)), t) | (n,d,s) <- readFix r,
						     (k,t)   <- readExp s] ++
	      [(0/0, t) | ("NaN", t) <- lex r] ++
	      [(1/0, t) | ("Infinity", t) <- lex r]
              where readFix r = [(read (ds++ds'), length ds', t) |
					  (ds,s)  <- lexDigits r,
					  (ds',t) <- lexDotDigits s ]

		    readExp (e:s) | e=='e' || e=='E' = readExp' s
                    readExp s			     = [(0,s)]

                    readExp' ('-':s) = [(-k,t) | (k,t) <- readDec s]
                    readExp' ('+':s) = readDec s
                    readExp' s	     = readDec s
		    
		    lexDigits = nonnull isDigit

		    lexDotDigits ('.':s@(c:_)) | isDigit c = lexDigits s
		    lexDotDigits ('.':s) = [("", s)]
		    lexDotDigits s = [("", s)]

