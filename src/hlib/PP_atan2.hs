module P_Prelude_atan2(atan2) where
{-# SPECIALIZE atan2 :: Double -> Double -> Double, Float -> Float -> Float #-}
atan2		:: (RealFloat a) => a -> a -> a
#if 0
atan2 y x	=  case (signum y, signum x) of
			( 0, 1) ->  0						--
			( 1, 0) ->  pi/2					--
			( 0,-1) ->  pi						--
			(-1, 0) -> -pi/2					--
			( _, 1) ->  atan (y/x)
			( _,-1) ->  atan (y/x) + pi
			( 0, 0) ->  error "Prelude.atan2: atan2 of origin"	--
#else
atan2 y x =
	if y == 0 then
	         if x > 0 then 0
	    else if x < 0 then pi
	    else {- x == 0 -}  error "Prelude.atan2: atan2 of origin"
	else if x == 0 then
	    if y > 0 then pi/2
	    else {- y < 0 -} -pi/2
	else if x > 0 then
	    atan (y/x)		-- 1st and 4th quadrant
	else {- x < 0 -}
	    if y > 0 then
	        atan (y/x) + pi	-- 2nd quadrant
	    else {- y < 0 -}
	        atan (y/x) - pi	-- 3rd quadrant
#endif
