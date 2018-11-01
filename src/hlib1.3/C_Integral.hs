module PreludeX(Integral(..)) where
class  (Enum a, Real a) => Integral a  where
    quot, rem, div, mod	:: a -> a -> a
    quotRem, divMod	:: a -> a -> (a,a)
    toInteger		:: a -> Integer
    toInt		:: a -> Int

    x `quot` y		=  q  where (q,r) = quotRem x y
    x `rem` y		=  r  where (q,r) = quotRem x y
    n `div` d           =  q  where (q,r) = divMod n d
    n `mod` d           =  r  where (q,r) = divMod n d
    divMod n d          =  if signum r == - signum d then (q-1, r+d) else qr
                           where qr@(q,r) = quotRem n d
    toInt x		=  {-:"PInteger2Int":-} (toInteger x)

