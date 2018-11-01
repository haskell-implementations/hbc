--@@ C printf style formatting.  Handles same types as printf in C, but requires the arguments
--@@ to be tagged.  Useful for formatting of floating point values.
--
-- A C printf like formatter.
-- Conversion specs:
--	-	left adjust
--	num	field width
--      *       as num, but taken from argument list
--	.	separates width from precision
-- Formatting characters:
-- 	c	Char, Int, Integer
--	d	Char, Int, Integer
--	o	Char, Int, Integer
--	x	Char, Int, Integer
--	u	Char, Int, Integer
--	f	Float, Double
--	g	Float, Double
--	e	Float, Double
--	s	String
--
module Printf(UPrintf(..), printf) where
import Array
import Char
--import LMLfmtf
import Numeric(showEFloat, showFFloat, showGFloat)

data UPrintf = UChar Char | UString String | UInt Int | UInteger Integer | UFloat Float | UDouble Double

printf :: String -> [UPrintf] -> String
printf ""       []       = ""
printf ""       (_:_)    = fmterr
printf ('%':'%':cs) us   = '%':printf cs us
printf ('%':_)  []       = argerr
printf ('%':cs) us@(_:_) = fmt cs us
printf (c:cs)   us       = c:printf cs us

fmt :: String -> [UPrintf] -> String
fmt cs us =
	let (width, prec, ladj, zero, cs', us') = getSpecs False False cs us
	    adjust (pre, str) = 
		let lstr = length str
		    lpre = length pre
		    fill = if lstr+lpre < width then take (width-(lstr+lpre)) (repeat (if zero then '0' else ' ')) else ""
		in  if ladj then pre ++ str ++ fill else if zero then pre ++ fill ++ str else fill ++ pre ++ str
        in
	case cs' of
	[]     -> fmterr
	c:cs'' ->
	    case us' of
	    []     -> argerr
	    u:us'' ->
		(case c of
		'c' -> adjust ("", [toEnum (toint u)])
		'd' -> adjust (fmti u)
		'x' -> adjust ("", fmtu 16 u)
		'o' -> adjust ("", fmtu 8  u)
		'u' -> adjust ("", fmtu 10 u)
		'e' -> adjust (dfmt' c prec u)
		'f' -> adjust (dfmt' c prec u)
		'g' -> adjust (dfmt' c prec u)
		's' -> adjust ("", tostr u)
		c   -> perror ("bad formatting char " ++ [c])
		) ++ printf cs'' us''

fmti (UInt i)     = if i < 0 then
			if i == -i then fmti (UInteger (toInteger i)) else ("-", itos (-i))
		    else
			("", itos i)
fmti (UInteger i) = if i < 0 then ("-", itos (-i)) else ("", itos i)
fmti (UChar c)    = fmti (UInt (fromEnum c))
fmti u		  = baderr

fmtu b (UInt i)     = if i < 0 then
			  if i == -i then itosb b (maxi - toInteger (i+1) - 1) else itosb b (maxi - toInteger (-i))
		      else
			  itosb b (toInteger i)
fmtu b (UInteger i) = itosb b i
fmtu b (UChar c)    = itosb b (toInteger (fromEnum c))
fmtu b u            = baderr

maxi :: Integer
maxi = (toInteger maxBound + 1) * 2

toint (UInt i)     = i
toint (UInteger i) = toInt i
toint (UChar c)    = fromEnum c
toint u		   = baderr

tostr (UString s) = s
tostr u		  = baderr

itos n = 
	if n < 10 then 
	    [toEnum (fromEnum '0' + toInt n)]
	else
	    let (q, r) = quotRem n 10 in
	    itos q ++ [toEnum (fromEnum '0' + toInt r)]

chars = array (0,15) (zipWith (,) [0..] "0123456789abcdef")
itosb :: Integer -> Integer -> String
itosb b n = 
	if n < b then 
	    [chars!n]
	else
	    let (q, r) = quotRem n b in
	    itosb b q ++ [chars!r]

stoi :: Int -> String -> (Int, String)
stoi a (c:cs) | isDigit c = stoi (a*10 + fromEnum c - fromEnum '0') cs
stoi a cs                 = (a, cs)

getSpecs :: Bool -> Bool -> String -> [UPrintf] -> (Int, Int, Bool, Bool, String, [UPrintf])
getSpecs l z ('-':cs) us = getSpecs True z cs us
getSpecs l z ('0':cs) us = getSpecs l True cs us
getSpecs l z ('*':cs) us = 
        case us of
        [] -> argerr
        nu : us' ->
	    let n = toint nu
		(p, cs'', us'') =
		    case cs of
                    '.':'*':r -> case us' of { [] -> argerr; pu:us'' -> (toint pu, r, us'') }
		    '.':r     -> let (n, cs') = stoi 0 r in (n, cs', us')
		    _         -> (-1, cs, us')
	    in  (n, p, l, z, cs'', us'')
getSpecs l z ('.':cs) us =
	let (p, cs') = stoi 0 cs
	in  (0, p, l, z, cs', us)
getSpecs l z cs@(c:_) us | isDigit c =
	let (n, cs') = stoi 0 cs
	    (p, cs'') = case cs' of
			'.':r -> stoi 0 r
			_     -> (-1, cs')
	in  (n, p, l, z, cs'', us)
getSpecs l z cs       us = (0, -1, l, z, cs, us)

--todbl (UDouble d) = d
--todbl (UFloat f)  = fromRational (toRational f)
--todbl u           = baderr
dfmt' c p (UDouble d) = dfmt c p d
dfmt' c p (UFloat f)  = dfmt c p f
dfmt' c p u           = baderr

dfmt c p d = 
--	case fmtf ("1" ++ (if p < 0 then "" else '.':itos p) ++ [c]) d of
	case (case c of 'e' -> showEFloat; 'f' -> showFFloat; 'g' -> showGFloat) 
               (if p < 0 then Nothing else Just p) d "" of
	'-':cs -> ("-", cs)
	cs     -> ("" , cs)

perror s = error ("Printf.printf: "++s)
fmterr = perror "formatting string ended prematurely"
argerr = perror "argument list ended prematurely"
baderr = perror "bad argument"
