module List where

infix 5 \\

elemIndex		:: Eq a => a -> [a] -> Maybe Int
elemIndex x xs		=  findIndex (x ==) xs

elemIndices		:: Eq a => a -> [a] -> [Int]
elemIndices x xs	=  findIndices (x ==) xs

find			:: (a -> Bool) -> [a] -> Maybe a
find p []		=  Nothing
find p (x:xs)		=  if p x then Just x else find p xs

findIndex		:: (a -> Bool) -> [a] -> Maybe Int
findIndex p xs		=
	case findIndices p xs of
	[]    -> Nothing
	x : _ -> Just x

findIndices             :: (a -> Bool) -> [a] -> [Int]
findIndices p xs        =  f 0 xs
  where f i []     = []
	f i (x:xs) = if p x then (i::Int) : f (i+1) xs else f (i+1) xs

-- List as set operations:
--   nub removes duplicates elements from a list.
--   delete, (\\), union and intersect preserve the invariant that 
--   lists don't contain duplicates.

-- nub (meaning "essence") remove duplicate elements from its list argument.
nub                     :: (Eq a) => [a] -> [a]
nub l			= nub' l []
	where nub' [] _	    = []
	      nub' (x:xs) l = if x `elem` l then nub' xs l else x : nub' xs (x:l)

nubBy                   :: (a -> a -> Bool) -> [a] -> [a]
nubBy eq []             =  []
nubBy eq (x:xs)         =  x : nubBy eq (filter (\y -> not (eq x y)) xs)

-- delete x removes the first occurrence of x from its list argument.
delete			:: (Eq a) => a -> [a] -> [a]
delete x []		= []
delete x (y:ys)		= if x == y then ys else y : delete x ys

deleteBy                :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy eq x []        = []
deleteBy eq x (y:ys)    = if x `eq` y then ys else y : deleteBy eq x ys

-- list difference (non-associative).  In the result of xs \\ ys,
-- the first occurrence of each element of ys in turn (if any)
-- has been removed from xs.  This (xs ++ ys) \\ xs == ys.
(\\)                    :: (Eq a) => [a] -> [a] -> [a]
bs \\ cs		=  flt bs cs
			   where [] `del` _	    = []
				 (x:xs) `del` y
					| x == y    = xs
					| otherwise = x : xs `del` y
				 flt z []     =  z
				 flt z (x:xs) =  flt (del z x) xs

deleteFirstsBy          :: (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy eq       =  foldl (flip (deleteBy eq))

union                   :: (Eq a) => [a] -> [a] -> [a]
union xs ys             =  xs ++ (ys \\ xs)

unionBy                 :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy eq xs ys        =  xs ++ foldl (flip (deleteBy eq)) ys xs

intersect               :: (Eq a) => [a] -> [a] -> [a]
intersect xs ys         =  [x | x <- xs, x `elem` ys]

intersectBy             :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy eq xs ys    =  [x | x <- xs, any (eq x) ys]

-- intersperse sep inserts sep between the elements of its list argument.
-- e.g. intersperse ',' "abcde" == "a,b,c,d,e"
intersperse             :: a -> [a] -> [a]
intersperse sep []      =  []
intersperse sep [x]     =  [x]
intersperse sep (x:xs)  =  x : sep : intersperse sep xs

transpose               :: [[a]] -> [[a]]
transpose               =  foldr
                             (\xs xss -> zipWith (:) xs (xss ++ repeat []))
                             []

-- partition takes a predicate and a list and returns a pair of lists:
-- those elements of the argument list that do and do not satisfy the
-- predicate, respectively; i.e.,
-- partition p xs == (filter p xs, filter (not . p) xs).
partition               :: (a -> Bool) -> [a] -> ([a],[a])
partition p xs		=  part xs
			   where part [] = ([], [])
			         part (x:xs) =
				     let (ts, fs) = part xs
				     in  if p x then
				     	     (x:ts, fs)
					 else
					     (ts, x:fs)

-- group splits its list argument into a list of lists of equal, adjacent
-- elements.  e.g.,
-- group "Mississippi" == ["M","i","ss","i","ss","i","pp","i"]
group                   :: (Eq a) => [a] -> [[a]]
group                   =  groupBy (==)

groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy eq []           =  []
groupBy eq (x:xs)       =  (x:ys) : groupBy eq zs
                           where (ys,zs) = span (eq x) xs

-- inits xs returns the list of initial segments of xs, shortest first.
-- e.g., inits "abc" == ["","a","ab","abc"]
inits                   :: [a] -> [[a]]
inits []                =  [[]]
inits (x:xs)            =  [[]] ++ map (x:) (inits xs)

-- tails xs returns the list of all final segments of xs, longest first.
-- e.g., tails "abc" == ["abc", "bc", "c",""]
tails                   :: [a] -> [[a]]
tails []                =  [[]]
tails xxs@(_:xs)        =  xxs : tails xs

isPrefixOf              :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _         =  True
isPrefixOf (x:xs) (y:ys)=  x == y || isPrefixOf xs ys

isSuffixOf              :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf x y          =  reverse x `isPrefixOf` reverse y

mapAccumL               :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccumL f s []        =  (s, [])
mapAccumL f s (x:xs)    =  (s'',y:ys)
                           where (s', y ) = f s x
                                 (s'',ys) = mapAccumL f s' xs

mapAccumR               :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccumR f s []        =  (s, [])
mapAccumR f s (x:xs)    =  (s'', y:ys)
                           where (s'',y ) = f s' x
                                 (s', ys) = mapAccumR f s xs

sort                    :: (Ord a) => [a] -> [a]
sort                    =  sortBy compare

-- "Natural" merge sort (stable).
sortBy                  :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp xs           =  tmsort xs
  where tmsort [] = []
	tmsort [x] = [x]		-- just for speed
	tmsort (x:xs) = msort (upSeq xs [x])

	upSeq [] xs = [reverse xs]
	upSeq (y:ys) xxs@(x:xs) =
	    if cmp x y <= EQ then
	        upSeq ys (y:xxs)
	    else
	        reverse xxs : upSeq ys [y]

	msort [xs] = xs
	msort xss = msort (mergePairs xss)

	mergePairs (xs:ys:xss) = merge xs ys : mergePairs xss
	mergePairs xss = xss

	merge xxs@(x:xs) yys@(y:ys) =
	    if cmp x y <= EQ then
	        x : merge xs yys
	    else
	        y : merge xxs ys
	merge []         yys = yys
	merge xxs        []  = xxs

insertBy                :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy cmp x []       =  [x]
insertBy cmp x ys@(y:ys') 
                        =  case cmp x y of
                           GT -> y : insertBy cmp x ys'
                           _  -> x : ys

maximumBy               :: (a -> a -> a) -> [a] -> a
maximumBy max []        =  error "List.maximumBy: empty list"
maximumBy max xs        =  foldl1 max xs

minimumBy               :: (a -> a -> a) -> [a] -> a
minimumBy min []        =  error "List.minimumBy: empty list"
minimumBy min xs        =  foldl1 min xs

genericLength           :: (Integral a) => [b] -> a
genericLength []        =  0
genericLength (x:xs)    =  1 + genericLength xs

genericTake             :: (Integral a) => a -> [b] -> [b]
genericTake _ []        =  []
genericTake n (x:xs) 
   | n > 0              =  x : genericTake (n-1) xs
   | otherwise          =  error "List.genericTake: negative argument"

genericDrop             :: (Integral a) => a -> [b] -> [b]
genericDrop 0 xs        =  xs
genericDrop _ []        =  []
genericDrop n (_:xs) 
   | n > 0              =  genericDrop (n-1) xs
   | otherwise          =  error "List.genericDrop: negative argument"

genericSplitAt          :: (Integral a) => a -> [b] -> ([b],[b])
genericSplitAt 0 xs     =  ([],xs)
genericSplitAt _ []     =  ([],[])
genericSplitAt n (x:xs) 
   | n > 0              =  (x:xs',xs'')
   | otherwise          =  error "List.genericSplitAt: negative argument"
       where (xs',xs'') =  genericSplitAt (n-1) xs

genericIndex            :: (Integral a) => [b] -> a -> b
genericIndex (x:_)  0   =  x
genericIndex (_:xs) n 
        | n > 0         =  genericIndex xs (n-1)
        | otherwise     =  error "List.genericIndex: negative argument"
genericIndex _ _        =  error "List.genericIndex: index too large"

genericReplicate        :: (Integral a) => a -> b -> [b]
genericReplicate n x    =  genericTake n (repeat x)
 
zip4			:: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4			=  zipWith4 (,,,)

zip5			:: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
zip5			=  zipWith5 (,,,,)

zip6			:: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a,b,c,d,e,f)]
zip6			=  zipWith6 (,,,,,)

zip7			:: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [(a,b,c,d,e,f,g)]
zip7			=  zipWith7 (,,,,,,)

zipWith4		:: (a->b->c->d->e) -> [a]->[b]->[c]->[d]->[e]
zipWith4 z (a:as) (b:bs) (c:cs) (d:ds)
			=  z a b c d : zipWith4 z as bs cs ds
zipWith4 _ _ _ _ _	=  []

zipWith5		:: (a->b->c->d->e->f) -> [a]->[b]->[c]->[d]->[e]->[f]
zipWith5 z (a:as) (b:bs) (c:cs) (d:ds) (e:es)
			=  z a b c d e : zipWith5 z as bs cs ds es
zipWith5 _ _ _ _ _ _	= []

zipWith6		:: (a->b->c->d->e->f->g) -> [a]->[b]->[c]->[d]->[e]->[f]->[g]
zipWith6 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs)
			=  z a b c d e f : zipWith6 z as bs cs ds es fs
zipWith6 _ _ _ _ _ _ _	= []

zipWith7		:: (a->b->c->d->e->f->g->h) -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]
zipWith7 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs)
		   =  z a b c d e f g : zipWith7 z as bs cs ds es fs gs
zipWith7 _ _ _ _ _ _ _ _ = []

unzip4			:: [(a,b,c,d)] -> ([a],[b],[c],[d])
unzip4			=  foldr (\(a,b,c,d) ~(as,bs,cs,ds) ->
					(a:as,b:bs,c:cs,d:ds))
				 ([],[],[],[])

unzip5			:: [(a,b,c,d,e)] -> ([a],[b],[c],[d],[e])
unzip5			=  foldr (\(a,b,c,d,e) ~(as,bs,cs,ds,es) ->
					(a:as,b:bs,c:cs,d:ds,e:es))
				 ([],[],[],[],[])

unzip6			:: [(a,b,c,d,e,f)] -> ([a],[b],[c],[d],[e],[f])
unzip6			=  foldr (\(a,b,c,d,e,f) ~(as,bs,cs,ds,es,fs) ->
					(a:as,b:bs,c:cs,d:ds,e:es,f:fs))
				 ([],[],[],[],[],[])

unzip7			:: [(a,b,c,d,e,f,g)] -> ([a],[b],[c],[d],[e],[f],[g])
unzip7			=  foldr (\(a,b,c,d,e,f,g) ~(as,bs,cs,ds,es,fs,gs) ->
					(a:as,b:bs,c:cs,d:ds,e:es,f:fs,g:gs))
				 ([],[],[],[],[],[],[])

