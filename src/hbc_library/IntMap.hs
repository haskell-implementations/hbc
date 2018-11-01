module IntMap (IntMap, iAddItem, iUnionMap, iLookup, iIsEmptyMap, iEmptyMap, iCardMap,
		intMapToList, listToIntMap, iAddItems, iAddCombine, iAssocDef, iApply) where
#ifndef __HASKELL_1_3
import Maybe
#endif

data IntMap a = Nil | Leaf Int{-#STRICT#-} a | Fork (IntMap a){-#STRICT#-} (IntMap a){-#STRICT#-}
instance (Text a) => Text (IntMap a) where
	showsType _ = showString "IntMap a"
	showsPrec _ Nil = showString "{}"
	showsPrec _ s = showString "{" . f (intMapToList s) . showString "}"
		where f [x] = g x
		      f (x:xs) = g x . showString ", " . f xs
		      g (i, r) = shows i . showString "->" . shows r

instance (Eq a) => Eq (IntMap a) where
	Nil == Nil = True
	Leaf x y == Leaf x' y' = x == x' && y == y'
	Fork l r == Fork l' r' = l == l' && r == r'
	_ == _ = False

iEmptyMap :: IntMap a
iEmptyMap = Nil

iIsEmptyMap :: IntMap a -> Bool
iIsEmptyMap Nil = True
iIsEmptyMap (Leaf _ _) = False
iIsEmptyMap (Fork _ _) = False

iAddItem :: Int -> a -> IntMap a -> IntMap a
iAddItem x y Nil = Leaf x y
iAddItem x y (Leaf x' y') =
	if x == x' then
	    Leaf x y
	else
	    iAddItem x y (iAddItem x' y' (Fork Nil Nil))
iAddItem x y (Fork l r) =
	if odd x then
	    Fork l (iAddItem (x `div` 2) y r)
	else
	    Fork (iAddItem (x `div` 2) y l) r

iAddItemKeep :: Int -> a -> IntMap a -> IntMap a
iAddItemKeep x y Nil = Leaf x y
iAddItemKeep x y t@(Leaf x' y') =
	if x == x' then
	    t
	else
	    iAddItemKeep x y (iAddItemKeep x' y' (Fork Nil Nil))
iAddItemKeep x y (Fork l r) =
	if odd x then
	    Fork l (iAddItemKeep (x `div` 2) y r)
	else
	    Fork (iAddItemKeep (x `div` 2) y l) r

iAssocDef :: IntMap a -> a -> Int -> a
iAssocDef Nil d x = if x==x then d else d			-- force it to be strict in x
iAssocDef (Leaf x' y) d x = if x == x' then y else d
iAssocDef (Fork l r) d x =
	if odd x then
	    iAssocDef r d (x `div` 2)
	else
	    iAssocDef l d (x `div` 2)

iLookup :: IntMap a -> Int -> Maybe a
iLookup Nil x = Nothing
iLookup (Leaf x' y) x = if x == x' then Just y else Nothing
iLookup (Fork l r) x =
	if odd x then
	    iLookup r (x `div` 2)
	else
	    iLookup l (x `div` 2)

iUnionMap :: IntMap a -> IntMap a -> IntMap a
iUnionMap Nil t = t
iUnionMap (Leaf x y) t = iAddItem x y t
iUnionMap t Nil = t
iUnionMap t (Leaf x y) = iAddItemKeep x y t
iUnionMap (Fork l r) (Fork l' r') = Fork (iUnionMap l l') (iUnionMap r r')

listToIntMap :: [(Int, a)] -> IntMap a
listToIntMap xs = foldr (\ (x,y) -> \ m -> iAddItem x y m) iEmptyMap xs

intMapToList :: IntMap a -> [(Int, a)]
intMapToList Nil = []
intMapToList (Leaf x y) = [(x, y)]
intMapToList (Fork l r) = [(2*x, y) | (x, y) <- intMapToList l] ++ [ (2*x+1, y) | (x, y) <- intMapToList r]

iCardMap :: IntMap a -> Int
iCardMap Nil = 0
iCardMap (Leaf _ _) = 1
iCardMap (Fork l r) = iCardMap l + iCardMap r

iAddCombine :: (a->a->a) -> Int -> a -> IntMap a -> IntMap a
iAddCombine comb x y Nil = Leaf x y
iAddCombine comb x y (Leaf x' y') =
	if x == x' then
	    Leaf x (comb y y')
	else
	    iAddCombine comb x y (iAddCombine comb x' y' (Fork Nil Nil))
iAddCombine comb x y (Fork l r) =
	if odd x then
	    Fork l (iAddCombine comb (x `div` 2) y r)
	else
	    Fork (iAddCombine comb (x `div` 2) y l) r

iAddItems t [] = t
iAddItems t ((x,y):xys) = iAddItems (iAddItem x y t) xys

iApply :: (a -> b) -> IntMap a -> IntMap b
iApply f Nil = Nil
iApply f (Leaf x y) = Leaf x (f y)
iApply f (Fork l r) = Fork (iApply f l) (iApply f r)
