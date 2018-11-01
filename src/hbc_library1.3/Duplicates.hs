module Duplicates where

--@@ Grouping of equal list elements.

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy eq [] = []
groupBy eq (x:xs) = let (ys,zs) = span (eq x) xs in (x:ys) : groupBy eq zs

uniqBy :: (a -> a -> Bool) -> [a] -> [a]
uniqBy eq = map head . groupBy eq

group :: (Eq a) => [a] -> [[a]]
group = groupBy (==)

uniq :: (Eq a) => [a] -> [a]
uniq = uniqBy (==)
