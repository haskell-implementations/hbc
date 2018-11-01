--
--@@ Simple pairing heaps as priority queues.<p>
--@@ Note: The ordering relation is given (explicitely
--@@ or implicitely) when an empty queue is created.
--
module PriorityQueue(
	PriorityQueue,
	empty, snoc, tail, head, null, singleton,
        emptyLE,
        merge,
	size,
	toList, fromList
	) where

data PriorityQueue a = Nil (a -> a -> Bool) | Node (a -> a -> Bool) (Tree a)

data Tree a = Fork a [Tree a]

empty :: (Ord a) => PriorityQueue a
empty = Nil (<=)

emptyLE :: (a -> a -> Bool) -> PriorityQueue a
emptyLE le = Nil le

snoc :: a -> PriorityQueue a -> PriorityQueue a
snoc a (Nil le) = Node le (Fork a [])
snoc a (Node le p) = Node le (merge' le p (Fork a []))

tail :: PriorityQueue a -> PriorityQueue a
tail (Nil _) = error "PriorityQueue.tail empty"
tail (Node le (Fork _ [])) = Nil le
tail (Node le (Fork _ ps)) = Node le (mergeAll le ps)

head :: PriorityQueue a -> a
head (Nil _) = error "PriorityQueue.head empty"
head (Node _ (Fork a _)) = a

null (Nil _) = True
null (Node _ _) = False

singleton :: (Ord a) => a -> PriorityQueue a
singleton x = Node (<=) (Fork x [])

merge :: PriorityQueue a -> PriorityQueue a -> PriorityQueue a
merge (Nil _) q = q
merge p (Nil _) = p
merge (Node le p) (Node _ q) = Node le (merge' le p q)

merge' :: (a -> a -> Bool) -> Tree a -> Tree a -> Tree a
merge' le p@(Fork x as) q@(Fork y bs) =
    if x `le` y then
        Fork x (q:as)
    else
        Fork y (p:bs)

mergeAll :: (a -> a -> Bool) -> [Tree a] -> Tree a
mergeAll _ [x] = x
mergeAll le [x,y] = merge' le x y
mergeAll le (x:y:xs) = merge' le (merge' le x y) (mergeAll le xs)

size :: PriorityQueue a -> Int
size (Nil _) = 0
size (Node _ p) = size' p
size' (Fork _ ps) = 1 + sum (map size' ps)

toList :: PriorityQueue a -> [a]
toList p = if null p then [] else head p : toList (tail p)

fromList :: (Ord a) => [a] -> PriorityQueue a
fromList [] = Nil (<=)
fromList xs = Node (<=) (mergeAll (<=) (map (\ x -> Fork x []) xs))

instance (Eq a) => Eq (PriorityQueue a) where
    p == q  =  toList p == toList q

instance (Show a) => Show (PriorityQueue a) where
    showsPrec p q = showParen (p > 0) (showString "PriorityQueue " . showsPrec 0 (toList q))
    showsType q = showString "(PriorityQueue " . showsType (f q) . showsType ")"
      where f :: PriorityQueue a -> a
            f = undefined
