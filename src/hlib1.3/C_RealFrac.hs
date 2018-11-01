module PreludeX(RealFrac(..)) where
class  (Fractional a, Real a) => RealFrac a  where
    properFraction	:: (Integral b) => a -> (b, a)
    truncate, round	:: (Integral b) => a -> b
    ceiling, floor	:: (Integral b) => a -> b
  
    truncate x		=  m  where (m,_) = properFraction x
    
    round x		=  let (n,r) = properFraction x
    			       m     = if r < 0 then n - 1 else n + 1
    			   in case signum (abs r - 0.5) of
    				-1 -> n
    			 	0  -> if even n then n else m
    				1  -> m
    
    ceiling x		=  if r > 0 then n + 1 else n
    			   where (n,r) = properFraction x
    
    floor x		=  if r < 0 then n - 1 else n
    			   where (n,r) = properFraction x

