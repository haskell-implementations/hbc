--
--@@ An implementation of Chris Okasaki's catenable lists.
--@@ These list have O(1) cons, head, tail, and (++)
--
module CatList(CatList,
	nil, empty, cons, snoc, singleton, 
        head, tail,
        length, size,
        null,
        toList, fromList,
	filter, reverse, foldr
        ) where
import qualified Queue as Q

data CatList a = Empty | NonEmpty (Tree a)
data Tree a = Node a (Q.Queue (Tree a))

link :: Tree a -> Tree a -> Tree a
link (Node x q) t2 = Node x (Q.snoc t2 q)

multiLink :: Q.Queue (Tree a) -> Tree a
multiLink q =
    if Q.null (Q.tail (Q.tail q)) then
        link (Q.head q) (Q.head (Q.tail q))
    else
        link (Q.head q) (multiLink (Q.tail q))

nil, empty :: CatList a
empty = Empty
nil = Empty

null :: CatList a -> Bool
null Empty = True
null (NonEmpty _) = False

singleton :: a -> CatList a
singleton x = NonEmpty (Node x Q.empty)

catenate :: CatList a -> CatList a -> CatList a
catenate Empty xs = xs
catenate xs Empty = xs
catenate (NonEmpty t1) (NonEmpty t2) = NonEmpty (link t1 t2)

cons :: a -> CatList a -> CatList a
cons x xs = catenate (singleton x) xs

snoc :: CatList a -> a -> CatList a
snoc xs x = catenate xs (singleton x)

head Empty = error "CatList.head empty"
head (NonEmpty (Node x _)) = x

tail Empty = error "CatList.tail empty"
tail (NonEmpty (Node x q)) =
    if Q.null q then
        Empty
    else if Q.null (Q.tail q) then
        NonEmpty (Q.head q)
    else
        NonEmpty (multiLink q)

length, size :: CatList a -> Int
length l = size l
size Empty = 0
size (NonEmpty t) = size' t
  where size' (Node _ q) = 1 + sum (map size' (Q.toList q))

toList Empty = []
toList (NonEmpty t) = toList' t
  where toList' (Node x q) = x : concatMap toList' (Q.toList q)

fromList l = Prelude.foldr cons empty l

instance (Eq a) => Eq (CatList a) where
    l1 == l2  =  toList l1 == toList l2

instance (Ord a) => Ord (CatList a) where
    compare l1 l2  =  compare (toList l1) (toList l2)

instance (Show a) => Show (CatList a) where
    showsType l = showString "(CatList " . showsType (f l) . showString ")"
      where f :: CatList a -> a
            f = undefined
    showsPrec p l = showsPrec p (toList l)

instance Functor CatList where
    map f Empty = Empty
    map f (NonEmpty t) = NonEmpty (map' t)
        where map' (Node x q) = Node (f x) (map map' q)

instance Monad CatList where
    xs >>= f = foldr catenate empty (map f xs)
    return x = singleton x

instance MonadZero CatList where
    zero = empty

instance MonadPlus CatList where
    xs ++ ys = catenate xs ys

--- Should be redone.
foldr :: (a -> b -> b) -> b -> CatList a -> b
foldr f z xs =
	if null xs then z else f (head xs) (foldr f z (tail xs))

reverse :: CatList a -> CatList a
reverse xs = rev xs empty
	where rev xs ys = if null xs then ys 
	                  else rev (tail xs) (cons (head xs) ys)

filter :: (a -> Bool) -> CatList a -> CatList a
filter p xs = 
	if null xs then empty else 
	let x = head xs in
	if p x then cons x (filter p (tail xs))
	else filter p (tail xs)
