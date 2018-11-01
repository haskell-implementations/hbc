module I_Text_numbers() where
import P_T_util

-- Standard real floating-point types

instance  Text Float  where
    readsPrec p = readSigned readFloat
    showsPrec   = showSigned showFloat
    showsType x = showString "Float"

instance  Text Double  where
    readsPrec p = readSigned readFloat
    showsPrec   = showSigned showFloat
    showsType x = showString "Double"

-- The functions readFloat and showFloat below use rational arithmetic
-- to insure correct conversion between the floating-point radix and
-- decimal.  It is often possible to use a higher-precision floating-
-- point type to obtain the same results.

readFloat:: (RealFloat a) => ReadS a
readFloat r = [(fromRational ((n%1)*10^^(k-d)), t) | (n,d,s) <- readFix r,
						     (k,t)   <- readExp s]
              where readFix r = [(read (ds++ds'), length ds', t)
					| (ds,'.':s) <- lexDigits r,
					  (ds',t)    <- lexDigits s ]

		    readExp (e:s) | e `elem` "eE" = readExp' s
                    readExp s			  = [(0,s)]

                    readExp' ('-':s) = [(-k,t) | (k,t) <- readDec s]
                    readExp' ('+':s) = readDec s
                    readExp' s	     = readDec s

-- The number of decimal digits m below is chosen to guarantee 
-- read (show x) = x.  See
--	Matula, D. W.  A formalization of floating-point numeric base
--	conversion.  IEEE Transactions on Computers C-19, 8 (1970 August),
--	681-692.
 
showFloat:: (RealFloat a) => a -> ShowS
showFloat x = if x < 0 then showChar '-' . showFloat' (-x) else showFloat' x
showFloat' x =
    if x == 0 then showString ("0." ++ take (m-1) (repeat '0'))
	      else if e >= m-1 || e < 0 then showSci else showFix
    where
    showFix	= showString whole . showChar '.' . showString frac
		  where (whole,frac) = splitAt (e+1) (show sig)
    showSci	= showChar d . showChar '.' . showString frac
		      . showChar 'e' . shows e
    		  where (d:frac) = show sig
    (m, sig, e) = if b == 10 then (w,  	s,   n+w-1)
		  	     else (m', sig', e'   )
    m'		= ceiling
		      (fromIntegral w * log (fromInteger b) / log 10 :: Double)
		  + 1

    (sig', e')	= if	  sig1 >= 10^m'     then (round (t/10), e1+1)
		  else if sig1 <  10^(m'-1) then (round (t*10), e1-1)
		  			    else (sig1,		e1  )
    sig1	= round t
    t		= s%1 * (b%1)^^n * 10^^(m'-e1-1)
    e1		= floor (logBase 10 x)
    (s, n)	= decodeFloat x
    b		= floatRadix x
    w		= floatDigits x

lexDigits		:: ReadS String	
lexDigits		=  nonnull isDigit

