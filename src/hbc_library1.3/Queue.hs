--
--@@ An implemenattion of Chris Okasaki's queues with O(1)
--@@ insert and delete.
--
module Queue(
	Queue,
	empty, snoc, tail, head, null,
	size,
	toList, fromList
	) where

data Queue a = Q [a] [a] [a]

instance (Eq a) => Eq (Queue a) where
    Q l r _ == Q l' r' _  =  l ++ reverse r == l' ++ reverse r'

instance (Show a) => Show (Queue a) where
    showsPrec p (Q l r _) = showParen (p > 0) (showString "Queue " . showsPrec 0 (l ++ reverse r))
    showsType (Q ~(x:_) _ _) = showString "(Queue " . showsType x . showsType ")"

empty :: Queue a
empty = Q [] [] []

snoc :: a -> Queue a -> Queue a
snoc x (Q l r l') = makeQ l (x:r) l'

head :: Queue a -> a
head (Q [] _ _) = error "Queue.head: empty queue"
head (Q (l:_) _ _) = l

tail :: Queue a -> Queue a
tail (Q [] _ _) = error "Queue.tail: empty queue"
tail (Q (_:l) r l') = makeQ l r l'

makeQ :: [a] -> [a] -> [a] -> Queue a
makeQ l r [] =
	let l' = rot l r []
	in  Q l' [] l'
makeQ l r (_:l') = Q l r l'

rot [] (r:_) a = r : a
rot (l:ls) (r:rs) a = l : rot ls rs (r:a)

null :: Queue a -> Bool
null (Q [] _ _) = True
null _ = False

size :: Queue a -> Int
size (Q l r _) = length l + length r

instance Functor Queue where
    --map :: (a -> b) -> Queue a -> Queue b
    map f (Q l r l') = Q (Prelude.map f l) (Prelude.map f r) (Prelude.map f l')

toList :: Queue a -> [a]
toList (Q l r _) = l ++ reverse r

fromList :: [a] -> Queue a
fromList l = foldr snoc empty (reverse l)
