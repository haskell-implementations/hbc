module RAList(RAList, (!!), update,
              cons, head, tail, null, nil, empty, singleton,
	      toList, fromList,
	      foldr, filter,
	      append, reverse, length, size) where
--
--@@ An implementation of Chris Okasaki's random access lists,
--@@ with O(1) head, tail, cons and O(log n) indexing.
--

data RAList a = RAN | RAC !Int (Tree a) (RAList a) deriving (Eq)
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq)

instance (Show a) => Show (RAList a) where
    showsPrec p xs = showsPrec p (toList xs)
    showsType xs = showString "(RAList " . showsType (head xs) . showString ")"
    -- readsPrec

nil, empty :: RAList a
empty = RAN
nil = RAN

null :: RAList a -> Bool
null RAN = True
null _ = False

singleton :: a -> RAList a
singleton x = cons x empty

cons :: a -> RAList a -> RAList a
cons x (RAC s1 t1 (RAC s2 t2 sts)) | s1 == s2 = 
	RAC (s1*2+1) (Node t1 x t2) sts
cons x sts = RAC 1 (Leaf x) sts

head :: RAList a -> a
head (RAC _ (Leaf x) _) = x
head (RAC _ (Node _ x _) _) = x
head _ = error "RAList.head: empty list"

tail :: RAList a -> RAList a
tail (RAC _ (Leaf _) sts) = sts
tail (RAC s (Node t1 _ t2) sts) = RAC s' t1 (RAC s' t2 sts)
	where s' = s `div` 2
tail _ = error "RAList.tail: empty list"

(!!) :: RAList a -> Int -> a
sts !! i = ix i sts
	where ix i RAN = emsg
	      ix i (RAC s t sts) =
		if i < s then
		    ixt i s t
		else
		    ix (i-s) sts
	      ixt :: Int -> Int -> Tree a -> a
	      ixt 0 _ (Leaf x) = x
	      ixt 0 _ (Node _ x _) = x
	      ixt i s (Node l _ r) =
	          let s' = s `div` 2
		  in  if i <= s' then
		          ixt (i-1) s' l
		      else
		          ixt (i-s'-1) s' r
	      ixt _ _ _ = emsg
	      emsg = error "RAList.!!: index out of bounds"

update :: RAList a -> Int -> a -> RAList a
update sts i x = up i sts
	where up i RAN = emsg
	      up i (RAC s t sts) =
		if i < s then
		    RAC s (upt i s x t) sts
		else
		    RAC s t (up (i-s) sts)
	      upt :: Int -> Int -> a -> Tree a -> Tree a
	      upt 0 _ x (Leaf _) = Leaf x
	      upt 0 _ x (Node l _ r) = Node l x r
	      upt i s x (Node l y r) =
	          let s' = s `div` 2
		  in  if i <= s' then
		          Node (upt (i-1) s' x l) y r
		      else
		          Node l y (upt (i-s'-1) s' x r)
	      upt _ _ _ _ = emsg
	      emsg = error "RAList.update: index out of bounds"

length, size :: RAList a -> Int
length sts = f sts 0
	where f RAN r = r
	      f (RAC s _ sts) r = f sts (s+r)
size sts = length sts

--instance (Eq a) => Eq (RAList a) where
--    l1 == l2  =  toList l1 == toList l2

instance (Ord a) => Ord (RAList a) where
    compare l1 l2  =  compare (toList l1) (toList l2)

instance Functor RAList where
    map f RAN = RAN
    map f (RAC s t sts) = RAC s (tmap t) (map f sts)
	where tmap (Leaf x) = Leaf (f x)
	      tmap (Node l x r) = Node (tmap l) (f x) (tmap r)

instance Monad RAList where
    xs >>= f = foldr catenate empty (map f xs)
    return x = singleton x

instance MonadZero RAList where
    zero = empty

instance MonadPlus RAList where
    xs ++ ys = catenate xs ys

catenate xs ys = foldr cons ys xs

-- The following functions could use the internal structure to
-- gain efficiency.
foldr :: (a -> b -> b) -> b -> RAList a -> b
foldr f z xs =
	if null xs then z else f (head xs) (foldr f z (tail xs))

toList :: RAList a -> [a]
toList xs = foldr (:) [] xs

fromList :: [a] -> RAList a
fromList xs = Prelude.foldr cons empty xs

append :: RAList a -> RAList a -> RAList a
append xs ys = 
	if null xs then ys else cons (head xs) (append (tail xs) ys)

reverse :: RAList a -> RAList a
reverse xs = rev xs empty
	where rev xs ys = if null xs then ys 
	                  else rev (tail xs) (cons (head xs) ys)

filter :: (a -> Bool) -> RAList a -> RAList a
filter p xs = 
	if null xs then empty else 
	let x = head xs in
	if p x then cons x (filter p (tail xs))
	else filter p (tail xs)
