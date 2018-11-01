module Maybe(
	fromJust, isJust, fromMaybe, listToMaybe, maybeToList,
	catMaybes, mapMaybe, unfoldr ) where

isJust			:: Maybe a -> Bool
isJust Nothing		=  False
isJust (Just _)		=  True

fromJust		:: Maybe a -> a
fromJust Nothing	=  error "Maybe.fromJust: Nothing"
fromJust (Just x)	=  x

fromMaybe		:: a -> Maybe a -> a
fromMaybe d Nothing	=  d
fromMaybe d (Just x)	=  x

listToMaybe		:: [a] -> Maybe a
listToMaybe []		=  Nothing
listToMaybe (a:as)	=  Just a

maybeToList		:: Maybe a -> [a]
maybeToList Nothing	=  []
maybeToList (Just x)	=  [x]

catMaybes		:: [Maybe a] -> [a]
catMaybes ms		=  [ x | Just x <- ms ]

mapMaybe		:: (a -> Maybe b) -> [a] -> [b]
mapMaybe f xs		=  [ x | x <- xs, Just x <- [f x] ]

--    unfoldr f' (foldr f z xs) == (xs,z)
--
-- if the following holds:
--
--    f' (f x y) = Just (x,y)
--    f' z       = Nothing
unfoldr			:: (a -> Maybe (b, a)) -> a -> (a,[b])
unfoldr f x =
    case f x of
    Just (y, x') -> let (x'', ys) = unfoldr f x' in (x'', y:ys)
    Nothing     -> (x, [])
