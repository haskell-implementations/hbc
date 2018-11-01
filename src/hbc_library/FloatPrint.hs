module FloatPrint where
data CutoffMode = CMNormal | CMAbsolute | CMRelative
dragon4 :: (Floating a) => Int -> ? -> a -> ? -> Int -> CutoffMode -> CutoffPlace -> [Int]
dragon4 b e f p bb cm cp =
   (if f = 0 then
	0
    else
	let rr = shift b f ((e-p) `max` 0)
	    ss = shift b 1 (0 `max` (p-e))
	    mn = shift b 1 ((e-p) `max` 0)
	    mp = mn
	in  fmtloop (fixup (rr ss mn mp)) ++ repeat (-1)
    where fmtloop (rr, ss, mn, mp) =
          fixup rr ss mn mp =
