module GenericList where
length          :: (Num i) => [b] -> i
length l        = fromInt (Prelude.length l)

drop		:: (Integral i) => i -> [a] -> [a]
drop i xs	= Prelude.drop (toInt i) xs

take		:: (Integral i) => i -> [a] -> [a]
take i xs	= Prelude.take (toInt i) xs

splitAt         :: (Integral i) => i -> [b] -> ([b],[b])
splitAt i xs	= Prelude.splitAt (toInt i) xs

replicate	:: (Integral i) => i -> a -> [a]
replicate n x	= Prelude.replicate (toInt n) x

(!!)		:: (Integral a) => [b] -> a -> b
xs !! i		= xs Prelude.!! (toInt i)
