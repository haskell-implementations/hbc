module PreludeX where

instance  RealFrac Double where
    properFraction x
        | n >= 0        =  (fromInteger m * fromInteger b ^ n, 0)
        | otherwise     =  (fromInteger w, encodeFloat r n)
                        where (m,n) = decodeFloat x
                              b     = floatRadix x
                              (w,r) = quotRem m (b^(-n))

--    ceiling x           = {-:"Dceil":-} x
--    floor x             = {-:"Dfloor":-} x

