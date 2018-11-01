module PreludeX where
zip3			:: [a] -> [b] -> [c] -> [(a,b,c)]
zip3 (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3 xs ys zs
zip3 _      _      _      = []
