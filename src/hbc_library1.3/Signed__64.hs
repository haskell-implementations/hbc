#include "bit.h"
module Signed__64(Signed64(..)) where
import LMLbitops

newtype Signed64 = Signed64 INT64 deriving (Eq, Ord)

instance Num Signed64 where
	Signed64 x + Signed64 y = Signed64 (SEXT64(x+y))
	Signed64 x - Signed64 y = Signed64 (SEXT64(x-y))
	Signed64 x * Signed64 y = Signed64 (SEXT64(x*y))
	negate (Signed64 x) = Signed64 (SEXT64(negate x))
	fromInteger x   = Signed64 (SEXT64(fromInteger x))
	fromInt x       = Signed64 (SEXT64(fromInt x))

instance Bounded Signed64 where
	minBound = Signed64 0x7fffffffffffffff
	maxBound = Signed64 (SEXT64(0x8000000000000000))

instance Enum Signed64 where
	toEnum n = Signed64 (fromInt n)
	fromEnum (Signed64 n) = toInt n
	enumFrom n = enumFromTo n maxBound
	enumFromThen n m = enumFromThenTo n m (if m > n then maxBound else minBound)

instance Real Signed64 where
	toRational (Signed64 n) = toRational (toInteger(n))

instance Integral Signed64 where
	toInteger (Signed64 n) = toInteger n
	toInt (Signed64 n) = toInt n
	-- Must handle negative numbers correctly, so convert to Integer
	quotRem (Signed64 x) (Signed64 y) = (Signed64 (fromInteger q), Signed64 (fromInteger r)) 
		where (q, r) = quotRem (toInteger(x)) (toInteger(y))
	divMod  (Signed64 x) (Signed64 y) = (Signed64 (fromInteger q), Signed64 (fromInteger r)) 
		where (q, r) = divMod  (toInteger(x)) (toInteger(y))

instance Show Signed64 where
	showsPrec p (Signed64 w) = showsPrec p (toInteger(w))
	showsType _ = showString "Signed64"

instance Read Signed64 where
	readsPrec p s = [(Signed64 (fromInteger n), s) | (n, s) <- readsPrec p s]
