#include "bit.h"
module Signed__16(Signed16(..)) where
import LMLbitops

newtype Signed16 = Signed16 Int deriving (Eq, Ord)

instance Num Signed16 where
	Signed16 x + Signed16 y = Signed16 (SEXT16(x+y))
	Signed16 x - Signed16 y = Signed16 (SEXT16(x-y))
	Signed16 x * Signed16 y = Signed16 (SEXT16(x*y))
	negate (Signed16 x)  = Signed16 (SEXT16(negate x))
	fromInteger x     = Signed16 (SEXT16(fromInteger x))
	fromInt x         = Signed16 (SEXT16(x))

instance Bounded Signed16 where
	minBound = Signed16 0x7fff
	maxBound = Signed16 (SEXT16(0x8000))

instance Enum Signed16 where
	toEnum n = Signed16 (SEXT16(n))
	fromEnum (Signed16 n) = n
	enumFrom n = enumFromTo n maxBound
	enumFromThen n m = enumFromThenTo n m (if m > n then maxBound else minBound)

instance Real Signed16 where
	toRational (Signed16 n) = toRational n

instance Integral Signed16 where
	toInteger (Signed16 n) = toInteger n
	toInt (Signed16 n) = n
	quotRem (Signed16 x) (Signed16 y) = (Signed16 q, Signed16 r) where (q, r) = quotRem x y
	divMod  (Signed16 x) (Signed16 y) = (Signed16 q, Signed16 r) where (q, r) = divMod  x y

instance Show Signed16 where
	showsPrec p (Signed16 w) = showsPrec p w
	showsType _ = showString "Signed16"

instance Read Signed16 where
	readsPrec p s = [(Signed16 n, s) | (n, s) <- readsPrec p s]
