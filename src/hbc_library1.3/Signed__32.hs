#include "bit.h"
module Signed__32(Signed32(..)) where
import LMLbitops

newtype Signed32 = Signed32 Int deriving (Eq, Ord)

instance Num Signed32 where
	Signed32 x + Signed32 y = Signed32 (SEXT32(x+y))
	Signed32 x - Signed32 y = Signed32 (SEXT32(x-y))
	Signed32 x * Signed32 y = Signed32 (SEXT32(x*y))
	negate (Signed32 x) = Signed32 (SEXT32(negate x))
	fromInteger x   = Signed32 (SEXT32(fromInteger x))
	fromInt x       = Signed32 (SEXT32(x))

instance Bounded Signed32 where
	minBound = Signed32 0x7fffffff
	maxBound = Signed32 (SEXT32(0x80000000))

instance Enum Signed32 where
	toEnum n = Signed32 (SEXT32(n))
	fromEnum (Signed32 n) = n
	enumFrom n = enumFromTo n maxBound
	enumFromThen n m = enumFromThenTo n m (if m > n then maxBound else minBound)

instance Real Signed32 where
	toRational (Signed32 n) = toRational (toInteger(n))

instance Integral Signed32 where
	toInteger (Signed32 n) = toInteger n
	toInt (Signed32 n) = n
	-- Must handle negative numbers correctly, so convert to Integer
	quotRem (Signed32 x) (Signed32 y) = (Signed32 (fromInteger q), Signed32 (fromInteger r)) 
		where (q, r) = quotRem (toInteger(x)) (toInteger(y))
	divMod  (Signed32 x) (Signed32 y) = (Signed32 (fromInteger q), Signed32 (fromInteger r)) 
		where (q, r) = divMod  (toInteger(x)) (toInteger(y))

instance Show Signed32 where
	showsPrec p (Signed32 w) = showsPrec p (toInteger(w))
	showsType _ = showString "Signed32"

instance Read Signed32 where
	readsPrec p s = [(Signed32 (fromInteger n), s) | (n, s) <- readsPrec p s]

