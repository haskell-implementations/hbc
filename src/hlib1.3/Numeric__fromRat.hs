module Numeric__fromRat(fromRat) where
import Array
import Ratio

-- Convert a Rational to a RealFloat.
-- The returned number is the closest RealFloat that
-- can be represented, using "round-to-even" if there
-- is a tie.
-- This is slow, but it should be accurate.

fromRat :: (RealFloat a) => Rational -> a
fromRat x = 
    if x == 0 then encodeFloat 0 0 		-- Handle exceptional cases
    else if x < 0 then - fromRat' (-x)		-- first.
    else fromRat' x

-- Conversion process:
-- Scale the rational number by the RealFloat base until
-- it lies in the range of the mantissa (as used by decodeFloat/encodeFloat).
-- Then round the rational to an Integer and encode it with the exponent
-- that we got from the scaling.
-- To speed up the scaling process we compute the log2 of the number to get
-- a first guess of the exponent.
fromRat' :: (RealFloat a) => Rational -> a
fromRat' x = r
  where b = floatRadix r
        p = floatDigits r
	(minExp0, _) = floatRange r
	minExp = minExp0 - p		-- the real minimum exponent
	xMin = toRational (expt b (p-1))
	xMax = toRational (expt b p)
	p0 = (integerLogBase b (numerator x) - integerLogBase b (denominator x) - p) `max` minExp
	f = if p0 < 0 then 1 % expt b (-p0) else expt b p0 % 1
	(x', p') = scaleRat (toRational b) minExp xMin xMax p0 (x / f)
	r = encodeFloat (round x') p'

-- Scale x until xMin <= x < xMax, or p (the exponent) <= minExp.
scaleRat :: Rational -> Int -> Rational -> Rational -> Int -> Rational -> (Rational, Int)
scaleRat b minExp xMin xMax p x =
    if p <= minExp then
        (x, p)
    else if x >= xMax then
        scaleRat b minExp xMin xMax (p+1) (x/b)
    else if x < xMin  then
        scaleRat b minExp xMin xMax (p-1) (x*b)
    else
        (x, p)

-- Exponentiation with a cache for the most common numbers.
minExpt = 0::Int
maxExpt = 1100::Int
expt :: Integer -> Int -> Integer
expt base n =
    if base == 2 && n >= minExpt && n <= maxExpt then
        expts!n
    else
        base^n
expts :: Array Int Integer
expts = array (minExpt,maxExpt) [(n,2^n) | n <- [minExpt .. maxExpt]]

-- Compute the (floor of the) log of i in base b.
-- Simplest way would be just divide i by b until it's smaller then b, but that would
-- be very slow!  We are just slightly more clever.
integerLogBase :: Integer -> Integer -> Int
integerLogBase b i =
     if i < b then
        0
     else
	-- Try squaring the base first to cut down the number of divisions.
        let l = 2 * integerLogBase (b*b) i
	    doDiv :: Integer -> Int -> Int
	    doDiv i l = if i < b then l else doDiv (i `div` b) (l+1)
	in  doDiv (i `div` (b^l)) l

