module IntSet (IntSet, iAddElem, iUnion, iElem, iIsEmpty, iEmpty, iRemElem, iSubset,
		iIntersection, iDifference, listToIntSet, intSetToList, iCard) where
import QSort

data IntSet = Nil | Leaf Int{-#STRICT#-} | Fork IntSet{-#STRICT#-} IntSet{-#STRICT#-}
instance Text IntSet where
	showsType _ = showString "IntSet"
	showsPrec _ Nil = showString "{}"
	showsPrec _ s = showString "{" . f (sort (intSetToList s)) . showString "}"
		where f [x] = shows x
		      f (x:xs) = shows x . showString ", " . f xs

instance Eq IntSet where
	Nil == Nil = True
	Leaf x == Leaf x' = x == x'
	Fork l r == Fork l' r' = l == l' && r == r'
	_ == _ = False

iEmpty :: IntSet
iEmpty = Nil

iIsEmpty :: IntSet -> Bool
iIsEmpty Nil = True
iIsEmpty (Leaf _) = False
iIsEmpty (Fork _ _) = False

iAddElem :: Int -> IntSet -> IntSet
iAddElem x Nil = Leaf x
iAddElem x s@(Leaf x') =
	if x == x' then
	    s
	else
	    iAddElem x (iAddElem x' (Fork Nil Nil))
iAddElem x (Fork l r) =
	if odd x then
	    Fork l (iAddElem (x `div` 2) r)
	else
	    Fork (iAddElem (x `div` 2) l) r

iElem :: Int -> IntSet -> Bool
iElem x Nil = False
iElem x (Leaf x') = x == x'
iElem x (Fork l r) =
	if odd x then
	    iElem (x `div` 2) r
	else
	    iElem (x `div` 2) l

iUnion :: IntSet -> IntSet -> IntSet
iUnion Nil t = t
iUnion (Leaf x) t = iAddElem x t
iUnion t Nil = t
iUnion t (Leaf x) = iAddElem x t
iUnion (Fork l r) (Fork l' r') = Fork (iUnion l l') (iUnion r r')

iRemElem :: Int -> IntSet -> IntSet
iRemElem x Nil = Nil
iRemElem x t@(Leaf x') = if x == x' then Nil else t
iRemElem x (Fork l r) =
	if odd x then
	    fork l (iRemElem (x `div` 2) r)
	else
	    fork (iRemElem (x `div` 2) l) r

fork Nil Nil = Nil
fork Nil (Leaf x) = Leaf (x*2+1)
fork (Leaf x) Nil = Leaf (x*2)
fork l r = Fork l r

iIntersection :: IntSet -> IntSet -> IntSet
iIntersection Nil _ = Nil
iIntersection t@(Leaf x) t' = if iElem x t' then t else Nil
iIntersection _ Nil = Nil
iIntersection t t'@(Leaf x) = if iElem x t then t' else Nil
iIntersection (Fork l r) (Fork l' r') = fork (iIntersection l l') (iIntersection r r')

listToIntSet :: [Int] -> IntSet
listToIntSet xs = foldr iAddElem iEmpty xs

intSetToList :: IntSet -> [Int]
intSetToList Nil = []
intSetToList (Leaf x) = [x]
intSetToList (Fork l r) = map (2*) (intSetToList l) ++ map ((1+).(2*)) (intSetToList r)

iSubset :: IntSet -> IntSet -> Bool
iSubset Nil _ = True
iSubset (Leaf x) t = iElem x t
iSubset (Fork l r) (Fork l' r') = iSubset l l' && iSubset r r'
iSubset _ _ = False

iDifference :: IntSet -> IntSet -> IntSet
iDifference t Nil = t
iDifference t (Leaf x) = iRemElem x t
iDifference Nil _ = Nil
iDifference t@(Leaf x) t' = if iElem x t' then Nil else t
iDifference (Fork l r) (Fork l' r') = fork (iDifference l l') (iDifference r r')

iCard :: IntSet -> Int
iCard Nil = 0
iCard (Leaf _) = 1
iCard (Fork l r) = iCard l + iCard r
