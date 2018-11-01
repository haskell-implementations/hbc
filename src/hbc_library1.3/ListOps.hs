module ListOps where
import List

limit :: Eq a => [a] -> a
limit = limitBy (==)

limitBy :: (a -> a -> Bool) -> [a] -> a
limitBy eq (x:y:zs) | x `eq` y  = x
                    | otherwise = limitBy eq (y:zs)

elemIndex   :: Eq a => [a] -> a -> Int
elemIndex = elemIndexBy (==)
elemIndexBy :: (a -> a -> Bool) -> [a] -> a -> Int
elemIndexBy eq [] x = error "ListOps.elemIndexBy: empty list"
elemIndexBy eq (x:xs) x' = if eq x x' then 0 else 1 + elemIndexBy eq xs x'

intersperse :: a -> [a] -> [a]
intersperse sep [] = []
intersperse sep [x] = [x]
intersperse sep (x:xs) = x : sep : (intersperse sep xs)

uniqBy :: (a -> a -> Bool) -> [a] -> [a]
uniqBy eq = map head . groupBy eq

uniq :: (Eq a) => [a] -> [a]
uniq = uniqBy (==)

deleteFirstsBy          :: (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy eq       =  foldl del
                           where [] `del` _         = []
                                 (x:xs) `del` y
                                        | x `eq` y    = xs
                                        | otherwise = x : xs `del` y

deleteFirsts		:: (Eq a) => [a] -> [a] -> [a]
deleteFirsts 		= (\\)

elemBy			:: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy eq _ []		= False
elemBy eq x (y:ys)	= eq x y || elemBy eq x ys

notElemBy eq x xs = not (elemBy eq x xs)

lookupBy :: (a -> a -> Bool) -> a -> [(a, b)] -> Maybe b
lookupBy eq key [] = Nothing
lookupBy eq key ((x,y):xys)
    | key `eq` x	= Just y
    | otherwise		= lookupBy eq key xys

products		:: (Num a) => [a] -> [a]
products l		=  scanl (*) 1 l

sums		:: (Num a) => [a] -> [a]
sums l		=  scanl (+) 0 l

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy eq [] = []
groupBy eq (x:xs) = let (ys,zs) = span (eq x) xs in (x:ys) : groupBy eq zs

group :: (Eq a) => [a] -> [[a]]
group = groupBy (==)

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [[]] ++ map (x:) (inits xs)

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = subsequences xs ++ map (x:) (subsequences xs)

tails :: [a] -> [[a]]
tails []         = []
tails xxs@(_:xs) = xxs : tails xs

permutations            :: [a] -> [[a]]
permutations []         =  [[]]
permutations (x:xs)     =  [zs | ys <- permutations xs, zs <- interleave x ys ]
  where interleave          :: a -> [a] -> [[a]]
        interleave x []     =  [[x]]
        interleave x (y:ys) =  (x:y:ys) : map (y:) (interleave x ys)

