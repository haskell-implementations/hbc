#include "bit.h"
module Signed__8(Signed8(..)) where
import LMLbitops

newtype Signed8 = Signed8 Int deriving (Eq, Ord)

instance Num Signed8 where
	Signed8 x + Signed8 y = Signed8 (SEXT8(x+y))
	Signed8 x - Signed8 y = Signed8 (SEXT8(x-y))
	Signed8 x * Signed8 y = Signed8 (SEXT8(x*y))
	negate (Signed8 x) = Signed8 (SEXT8(negate x))
	fromInteger x   = Signed8 (SEXT8(fromInteger x))
	fromInt x       = Signed8 (SEXT8(x))

instance Bounded Signed8 where
	minBound = Signed8 0x7f
	maxBound = Signed8 (SEXT8(0x80))

instance Enum Signed8 where
	toEnum n = Signed8 (SEXT8(n))
	fromEnum (Signed8 n) = n
	enumFrom n = enumFromTo n maxBound
	enumFromThen n m = enumFromThenTo n m (if m > n then maxBound else minBound)

instance Real Signed8 where
	toRational (Signed8 n) = toRational n

instance Integral Signed8 where
	toInteger (Signed8 n) = toInteger n
	toInt (Signed8 n) = n
	quotRem (Signed8 x) (Signed8 y) = (Signed8 q, Signed8 r) where (q, r) = quotRem x y
	divMod  (Signed8 x) (Signed8 y) = (Signed8 q, Signed8 r) where (q, r) = divMod  x y

instance Show Signed8 where
	showsPrec p (Signed8 w) = showsPrec p w
	showsType _ = showString "Signed8"

instance Read Signed8 where
	readsPrec p s = [(Signed8 n, s) | (n, s) <- readsPrec p s]
