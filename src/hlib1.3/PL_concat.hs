module PreludeX(concat) where
concatList		:: [[a]] -> [a]
concatList []		= []
concatList ([]:xss)	= concatList xss			-- for better stack behaviour!
concatList (xs:xss)	= xs ++ concatList xss

{-# SPECIALIZE concat :: [[a]] -> [a] = concatList #-}
concat :: MonadPlus m => [m a] -> m a
concat [] = zero
concat (xs:xss) = xs ++ concat xss

