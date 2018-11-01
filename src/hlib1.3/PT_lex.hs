module PreludeX(lex) where
import Char(isSpace, isAlpha, isDigit, isAlphanum, isHexDigit)
-- SLOW
lex 	    		:: ReadS String
lex ""			= [("","")]
lex (c:s) | isSpace c	= lex (dropWhile isSpace s)
lex ('\'':s)		= [('\'':ch++"'", t) | (ch,'\'':t)  <- lexLitChar s,
					       ch /= "'"		]
lex ('"':s)		= [('"':str, t)      | (str,t) <- lexString s]
			  where
			  lexString ('"':s) = [("\"",s)]
			  lexString s = [(ch++str, u)
						| (ch,t)  <- lexStrItem s,
						  (str,u) <- lexString t  ]

			  lexStrItem ('\\':'&':s) = [("\\&",s)]
			  lexStrItem ('\\':c:s) | isSpace c
			      = [("\\&",t) | '\\':t <- [dropWhile isSpace s]]
			  lexStrItem s		  = lexLitChar s

lex (c:s) | isSingle c	= [([c],s)]
	  | isSym c	= [(c:sym,t)	     | (sym,t) <- [span isSym s]]
	  | isAlpha c	= [(c:nam,t)	     | (nam,t) <- [span isIdChar s]]
	  | isDigit c	= [(c:ds++fe,t)	     | (ds,s)  <- [span isDigit s],
					       (fe,t)  <- lexFracExp s	   ]
	  | otherwise	= []	-- bad character
		where
		isSingle c  =  c `elem` ",;()[]{}_`"
		isSym c	    =  c `elem` "-~!@#$%&*+./<=>?\\^|:¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿×÷"
		isIdChar c  =  isAlphanum c || c `elem` "_'"

		lexFracExp ('.':c:cs) | isDigit c
                                    = [('.':ds++e,u) | (ds,t) <- lexDigits (c:cs),
                                                       (e,u)  <- lexExp t]
		lexFracExp s          = [("",s)]

		lexExp (e:s) | e == 'e' || e == 'E'
			 = [(e:c:ds,u) | (c:t)	<- [s], c == '+' || c == '-',
						   (ds,u) <- lexDigits t] ++
			   [(e:ds,t)   | (ds,t)	<- lexDigits s]
		lexExp s = [("",s)]

lexDigits		:: ReadS String	
lexDigits		=  nonnull isDigit

lexDotDigits		:: ReadS String
lexDotDigits ('.':s@(c:_)) | isDigit c = [('.':ds, t) | (ds, t) <- lexDigits s]
lexDotDigits ('.':s) 	=  [("", s)]
lexDotDigits s 		=  [("", s)]

nonnull			:: (Char -> Bool) -> ReadS String
nonnull p s		=  [(cs,t) | (cs@(_:_),t) <- [span p s]]

lexLitChar		:: ReadS String
lexLitChar ('\\':s)	=  [('\\':esc, t) | (esc,t) <- lexEsc s]
	where
	lexEsc (c:s)	 | c `elem` "abfnrtv\\\"'" = [([c],s)]
	lexEsc ('^':c:s) | c >= '@' && c <= '_'  = [(['^',c],s)]
	lexEsc s@(d:_)	 | isDigit d		 = lexDigits s
	lexEsc _	=  []
lexLitChar (c:s)	=  [([c],s)]
lexLitChar ""		= []

