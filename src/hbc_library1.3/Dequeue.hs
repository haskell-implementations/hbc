--
--@@ An implemenation of Chris Okasaki's dequeues with O(1)
--@@ insert and delete.
--@@ See JFP vol 5 part 4.
--
module Dequeue (
	Dequeue,
	empty, snoc, tail, head, null,
	cons, init, last,
	size,
	toList, fromList
	) where

data Dequeue a = Q [a] [a] [a] [a] !Int !Int

instance (Eq a) => Eq (Dequeue a) where
    Q l r _ _ _ _ == Q l' r' _ _ _ _  =  l ++ reverse r == l' ++ reverse r'

instance (Show a) => Show (Dequeue a) where
    showsPrec p (Q l r _ _ _ _) = showParen (p > 0) (showString "Dequeue " . showsPrec 0 (l ++ reverse r))
    showsType (Q ~(x:_) _ _ _ _ _) = showString "(Dequeue " . showsType x . showsType ")"

c = 3 :: Int

empty :: Dequeue a
empty = Q [] [] [] [] 0 0

snoc :: a -> Dequeue a -> Dequeue a
snoc x (Q l r l' r' ln rn) = 
	tl1 l' $ \ l' ->
	tl1 r' $ \ r' ->
	makeQ l (x:r) l' r' ln (rn+1)

cons :: a -> Dequeue a -> Dequeue a
cons x (Q l r l' r' ln rn) = 
	tl1 l' $ \ l' ->
	tl1 r' $ \ r' ->
	makeQ (x:l) r l' r' (ln+1) rn

head :: Dequeue a -> a
head (Q [] [] _ _ _ _) = error "Dequeue.head: empty queue"
head (Q [] (x:r) _ _ _ _) = x
head (Q (x:_) _ _ _ _ _) = x

tail :: Dequeue a -> Dequeue a
tail (Q [] [] _ _ _ _) = error "Dequeue.tail: empty queue"
tail (Q [] _ _ _ _ _) = empty
tail (Q (_:l) r r' l' ln rn) = 
	tl2 l' $ \ l' ->
	tl2 r' $ \ r' ->
	makeQ l r r' l' (ln-1) rn

last :: Dequeue a -> a
last (Q [] [] _ _ _ _) = error "Dequeue.last: empty queue"
last (Q (x:r) [] _ _ _ _) = x
last (Q _ (x:_) _ _ _ _) = x

init :: Dequeue a -> Dequeue a
init (Q [] [] _ _ _ _) = error "Dequeue.init: empty queue"
init (Q _ [] _ _ _ _) = empty
init (Q l (_:r) r' l' ln rn) = 
	tl2 l' $ \ l' ->
	tl2 r' $ \ r' ->
	makeQ l r r' l' ln (rn-1)

makeQ :: [a] -> [a] -> [a] -> [a] -> Int -> Int -> Dequeue a
makeQ l r l' r' ln rn 
	| ln > c*rn + 1 = 
		let n = (ln+rn) `quot` 2
		    l' = take n l
		    r' = rot1 n r l
		in  Q l' r' l' r' (ln-n) (rn+n)
	| rn > c*ln + 1 =
		let n = (ln+rn) `quot` 2
		    l' = rot1 n l r
		    r' = take n r
		in  Q l' r' l' r' (ln+n) (rn-n)
	| otherwise     = Q l r l' r' ln rn

rot1 n l r =
	if n >= c then
	    case l of x:xs -> x : rot1 (n-c) xs (drop c r)
	else
	    rot2 l (drop n r) []

rot2 [] r a = rev r a
rot2 l r a | lessLength c r = l ++ rev r a
rot2 (x:l) r a = x : rot2 l (drop c r) (reverse (take c r) ++ a)

rev [] a = a
rev (x:xs) a = rev xs (x:a)

lessLength :: Int -> [a] -> Bool
lessLength n [] = False
lessLength 0 (_:_) = True
lessLength n (_:xs) = lessLength (n-1) xs

-- Take the tail, but avoid failing.
tl1 [] c = c []
tl1 (_:xs) c = c xs
tl2 [] c = c []
tl2 [_] c = c []
tl2 (_:_:xs) c = c xs

null :: Dequeue a -> Bool
null (Q [] [] _ _ _ _) = True
null _ = False

size :: Dequeue a -> Int
size (Q _ _ _ _ ln rn) = ln + rn

instance Functor Dequeue where
    --map :: (a -> b) -> Dequeue a -> Dequeue b
    map f (Q l r l' r' ln rn) = Q (Prelude.map f l) (Prelude.map f r) (Prelude.map f l') (Prelude.map f r') ln rn

toList :: Dequeue a -> [a]
toList (Q l r _ _ _ _) = l ++ reverse r

fromList :: [a] -> Dequeue a
fromList l = foldr cons empty l
