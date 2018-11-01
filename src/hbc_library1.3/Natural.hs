module Natural(Natural(..)) where
-- Phil Wadler's Nat library

data  Natural = Zero | Succ !Natural  deriving  (Eq, Ord)

instance Show Natural where
  showsPrec p n		=  showsPrec p (toInt n)

instance Read Natural where
  readsPrec p s		=  [ (fromInt i, s') |
			     (i,s') <- readsPrec p s ]

instance Num Natural where
  Zero + n  		=  n
  Succ m + n		=  Succ (m+n)

  Zero * n		=  Zero
  Succ m * n		=  n + (m*n)

  m - Zero		=  m
  Succ m - Succ n	=  m-n
  Zero - _		=  error "Natural.(-) < 0"

  fromInt 0  		=  Zero
  fromInt i | i > 0	=  Succ (fromInt (i-1))
  fromInt _		=  error "Natural.fromInt: n < 0"

  fromInteger		=  fromInt . fromInteger

instance Real Natural where
  toRational n  	=  toRational (toInteger n)

instance Enum Natural where
  enumFrom n  		=  n : enumFrom (Succ n)
  enumFromThen n m	=  iterate (+(m-n)) n

instance Ix Natural where
  range (l,u)		=  [l..u]
  index (l,u) n		=  toInt (n-l)
  inRange (l,u) n	=  l <= n && n <= u
  
instance Integral Natural where
  quotRem n d | n < d	=  (0, n)
              | n >= d	=  let (q,r) = quotRem (n-d) d  in (q+1,r)
  divMod n d		=  quotRem n d

  toInt n		=  toint n 0
	where toint :: Natural -> Int -> Int
	      toint Zero r = r
	      toint (Succ n) r = toint n (r+1)

  toInteger		=  toInteger . toInt
