module Ix(Ix(..), rangeSize) where

{-
import Ix__Char
import Ix__Int
import Ix__Integer
import Ix__Bool
-}

class  (Ord a) => Ix a  where
    range		:: (a,a) -> [a]
    index		:: (a,a) -> a -> Int
    inRange		:: (a,a) -> a -> Bool

instance Ix Char where
	range (l,h) = [l .. h]
	index (l,h) i = if l <= i && i <= h then fromEnum i - fromEnum l else error ("Ix.Char.index: Index out of range " ++ show (l,h) ++ " " ++ show i)
	inRange (l,h) i = l <= i && i <= h

instance Ix Int where
	range (l,h) = [l .. h]
	index (l,h) x = if l <= x && x <= h then x - l else error ("Ix.Int.index: Index out of range " ++ show (l,h) ++ " " ++ show x)
	inRange (l,h) x = l <= x && x <= h

instance Ix Integer where
	range (l,h) = [l .. h]
	index (l,h) x = if l <= x && x <= h then fromInteger (x - l) else error ("Ix.Integer.index: Index out of range " ++ show (l,h) ++ " " ++ show x)
	inRange (l,h) x = l <= x && x <= h

instance Ix Bool where
	range (False,True) = [False,True]
        range (True,False) = []
        range (x,_)        = [x]
        index (False,_) True  = 1
        index (_,_)     _     = 0
        inRange (False,True) _ = True
        inRange (True,False) _ = False
        inRange (x,_)        y = x == y

rangeSize :: Ix a => (a,a) -> Int
rangeSize lh@(l,h) = index lh h + 1
