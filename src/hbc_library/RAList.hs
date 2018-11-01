module RAList(RAList, raIndex, raUpdate,
              raCons, raHead, raTail, raNull, raNil,
	      raToList, listToRa,
	      raFoldr, raMap, raFilter,
	      raAppend, raReverse, raLength) where
--
-- An implementation of Chris Okasaki's random access lists.
--

data RAList a = RAN | RAC Int  {-# STRICT #-} (Tree a) (RAList a) deriving (Eq)
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq)

instance (Text a) => Text (RAList a) where
    showsPrec p xs = showsPrec p (raToList xs)
    showsType xs = showString "(RAList " . showsType (raHead xs) . showString ")"
    -- readsPrec

raNil :: RAList a
raNil = RAN

raNull :: RAList a -> Bool
raNull RAN = True
raNull _ = False

raCons :: a -> RAList a -> RAList a
raCons x (RAC s1 t1 (RAC s2 t2 sts)) | s1 == s2 = 
	RAC (s1*2+1) (Node t1 x t2) sts
raCons x sts = RAC 1 (Leaf x) sts

raHead :: RAList a -> a
raHead (RAC _ (Leaf x) _) = x
raHead (RAC _ (Node _ x _) _) = x
raHead _ = error "raHead: empty list"

raTail :: RAList a -> RAList a
raTail (RAC _ (Leaf _) sts) = sts
raTail (RAC s (Node t1 _ t2) sts) = RAC s' t1 (RAC s' t2 sts)
	where s' = s `div` 2
raTail _ = error "raTail: empty list"

raIndex :: RAList a -> Int -> a
raIndex sts i = ix i sts
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
	      emsg = error "raIndex: index out of bounds"

raUpdate :: RAList a -> Int -> a -> RAList a
raUpdate sts i x = up i sts
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
	      emsg = error "raUpdate: index out of bounds"

raLength :: RAList a -> Int
raLength sts = f sts 0
	where f RAN r = r
	      f (RAC s _ sts) r = f sts (s+r)

raMap :: (a -> b) -> RAList a -> RAList b
raMap f RAN = RAN
raMap f (RAC s t sts) = RAC s (tmap t) (raMap f sts)
	where tmap (Leaf x) = Leaf (f x)
	      tmap (Node l x r) = Node (tmap l) (f x) (tmap r)

-- The following functions could use the internal structure to
-- gain efficiency.
raFoldr :: (a -> b -> b) -> b -> RAList a -> b
raFoldr f z xs =
	if raNull xs then z else f (raHead xs) (raFoldr f z (raTail xs))

raToList :: RAList a -> [a]
raToList xs = raFoldr (:) [] xs

listToRa :: [a] -> RAList a
listToRa xs = foldr raCons raNil xs

raAppend :: RAList a -> RAList a -> RAList a
raAppend xs ys = 
	if raNull xs then ys else raCons (raHead xs) (raAppend (raTail xs) ys)

raReverse :: RAList a -> RAList a
raReverse xs = rev xs raNil
	where rev xs ys = if raNull xs then ys 
	                  else rev (raTail xs) (raCons (raHead xs) ys)

raFilter :: (a -> Bool) -> RAList a -> RAList a
raFilter p xs = 
	if raNull xs then raNil else 
	let x = raHead xs in
	if p x then raCons x (raFilter p (raTail xs))
	else raFilter p (raTail xs)
