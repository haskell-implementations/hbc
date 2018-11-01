module Numeric__formatRealFloat where
import Char
import Array

data FFFormat = FFExponent | FFFixed | FFGeneric
    deriving (Eq, Ord, Show)

formatRealFloat :: (RealFloat a) => FFFormat -> Maybe Int -> a -> String
formatRealFloat fmt decs x = s
  where base = 10
	s = if isNaN x then 
		"NaN"
	    else if isInfinite x then 
	    	if x < 0 then "-Infinity" else "Infinity"
	    else if x < 0 || isNegativeZero x then 
		'-' : doFmt fmt (floatToDigits (toInteger base) (-x))
	    else 
		doFmt fmt (floatToDigits (toInteger base) x)
	doFmt fmt (is, e) =
	    let ds = map intToDigit is
	    in  case fmt of
		FFGeneric -> 
		    doFmt (if e < 0 || e > 7 then FFExponent else FFFixed) (is, e)
		FFExponent ->
		    case decs of
		    Nothing ->
			case ds of
                         "0" -> "0.0e0"
			 [d] -> d : ".0e" ++ show (e-1)
			 d:ds -> d : '.' : ds ++ "e" ++ show (e-1)
		    Just dec ->
		        let dec' = max dec 1 in
			let (ei, is') = roundTo base (dec'+1) is
			    d:ds = map intToDigit (if ei > 0 then init is' else is')
			in  d:'.':ds  ++ "e" ++ show (e-1+ei)
		FFFixed ->
		    case decs of
		    Nothing ->
			let f 0 s ds = mk0 s ++ "." ++ mk0 ds
			    f n s "" = f (n-1) (s++"0") ""
			    f n s (d:ds) = f (n-1) (s++[d]) ds
			    mk0 "" = "0"
			    mk0 s = s
			in  f e "" ds
		    Just dec ->
		        let dec' = max dec 1 in
			if e >= 0 then
			    let (ei, is') = roundTo base (dec' + e) is
			        (ls, rs) = splitAt (e+ei) (map intToDigit is')
			    in  (if null ls then "0" else ls) ++ 
			        (if null rs then "" else '.' : rs)
			else
			    let (ei, is') = roundTo base dec' (replicate (-e) 0 ++ is)
			        d : ds = map intToDigit (if ei > 0 then is' else 0:is')
			    in  d : '.' : ds

roundTo :: Int -> Int -> [Int] -> (Int, [Int])
roundTo base d is = case f d is of
		(0, is) -> (0, is)
		(1, is) -> (1, 1 : is)
  where b2 = base `div` 2
	f n [] = (0, replicate n 0)
	f 0 (i:_) = (if i >= b2 then 1 else 0, [])
	f d (i:is) = 
	    let (c, ds) = f (d-1) is
	        i' = c + i
	    in  if i' == base then (1, 0:ds) else (0, i':ds)

--
-- Based on "Printing Floating-Point Numbers Quickly and Accurately"
-- by R.G. Burger and R. K. Dybvig, in PLDI 96.
-- This version uses a much slower logarithm estimator.  It should be improved.
--
floatToDigits :: (RealFloat a) => Integer -> a -> ([Int], Int)
floatToDigits _ 0.0 = ([0], 0)
floatToDigits base x =
    let (f0, e0) = decodeFloat x
	(minExp0, _) = floatRange x
	p = floatDigits x
	b = floatRadix x
	minExp = minExp0 - p		-- the real minimum exponent
	-- Haskell requires that f be adjusted so denormalized numbers
	-- will have an impossibly low exponent.  Adjust for this.
	(f, e) = let n = minExp - e0
	         in  if n > 0 then (f0 `div` (b^n), e0+n) else (f0, e0)
	(r, s, mUp, mDn) =
           if e >= 0 then
	       let be = b^e in
	       if f == b^(p-1) then
	           (f*be*b*2, 2*b, be*b, b)
	       else
	           (f*be*2, 2, be, be)
	   else
	       if e > minExp && f == b^(p-1) then
	           (f*b*2, b^(-e+1)*2, b, 1)
	       else
	           (f*2, b^(-e)*2, 1, 1)
	k = 
	    let	k0 =
	    	    if b==2 && base==10 then
		        -- logBase 10 2 is slightly bigger than 3/10 so
			-- the following will err on the low side.  Ignoring
			-- the fraction will make it err even more.
			-- Haskell promises that p-1 <= logBase b f < p.
			(p - 1 + e0) * 3 `div` 10
		    else
		        ceiling ((log (fromInteger (f+1)) + fromInt e * log (fromInteger b)) / 
                                  log (fromInteger base))
	        fixup n =
		    if n >= 0 then
		        if r + mUp <= expt base n * s then n else fixup (n+1)
		    else
		        if expt base (-n) * (r + mUp) <= s then n else fixup (n+1)
	    in  fixup k0

	gen ds rn sN mUpN mDnN =
	    let (dn, rn') = (rn * base) `divMod` sN
	        mUpN' = mUpN * base
	        mDnN' = mDnN * base
	    in  case (rn' < mDnN', rn' + mUpN' > sN) of
	        (True,  False) -> dn : ds
		(False, True)  -> dn+1 : ds
		(True,  True)  -> if rn' * 2 < sN then dn : ds else dn+1 : ds
		(False, False) -> gen (dn:ds) rn' sN mUpN' mDnN'
	rds =
	    if k >= 0 then
	        gen [] r (s * expt base k) mUp mDn
	    else
	        let bk = expt base (-k)
	        in  gen [] (r * bk) s (mUp * bk) (mDn * bk)
    in	(map toInt (reverse rds), k)

minExpt = 0::Int
maxExpt = 325::Int
expt :: Integer -> Int -> Integer
expt base n =
    if base == 10 && n >= minExpt && n <= maxExpt then
        expts!n
    else
        base^n
expts :: Array Int Integer
expts = array (minExpt,maxExpt) [(n,10^n) | n <- [minExpt .. maxExpt]]
