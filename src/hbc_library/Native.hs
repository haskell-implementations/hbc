module Native(Native(..), Bytes(..), Maybe.., shortIntToBytes, bytesToShortInt, longIntToBytes, bytesToLongInt, showB, readB) where

import Maybe
import LMLbyteops

type Bytes = [Char]

class Native a where
    showBytes     :: a -> Bytes -> Bytes	        -- convert to bytes
    listShowBytes :: [a] -> Bytes -> Bytes	        -- convert a list to bytes
    readBytes     :: Bytes -> Maybe (a, Bytes)	        -- get an item and the rest
    listReadBytes :: Int -> Bytes -> Maybe ([a], Bytes) -- get n items and the rest

    listShowBytes []     bs = bs
    listShowBytes (x:xs) bs = showBytes x (listShowBytes xs bs)
    listReadBytes 0 bs = Just ([], bs)
    listReadBytes n bs = 
	case readBytes bs of
	Nothing -> Nothing
	Just (x,bs') ->
		case listReadBytes (n-1) bs' of
		Nothing -> Nothing
		Just (xs,bs'') -> Just (x:xs, bs'')

hasNElems :: Int -> [a] -> Bool
hasNElems 0 _      = True
hasNElems 1 (_:_)  = True		-- speedup
hasNElems 2 (_:_:_)  = True		-- speedup
hasNElems 3 (_:_:_:_)  = True		-- speedup
hasNElems 4 (_:_:_:_:_)  = True		-- speedup
hasNElems _ []     = False
hasNElems n (_:xs) = hasNElems (n-1) xs

lenLong   = length (longToBytes   0 [])
lenInt    = length (intToBytes    0 [])
lenShort  = length (shortToBytes  0 [])
lenFloat  = length (floatToBytes  0 [])
lenDouble = length (doubleToBytes 0 [])

instance Native Char where
    showBytes	c bs = c:bs
    readBytes [] = Nothing
    readBytes (c:cs) = Just (c,cs)
    listReadBytes n bs = f n bs []
	where f 0 bs cs = Just (reverse cs, bs)
	      f _ [] _  = Nothing
	      f n (b:bs) cs = f (n-1::Int) bs (b:cs)

instance Native Int where
    showBytes i bs = intToBytes i bs
    readBytes bs = if hasNElems lenInt bs then Just (bytesToInt bs) else Nothing

instance Native Float where
    showBytes i bs = floatToBytes i bs
    readBytes bs = if hasNElems lenFloat bs then Just (bytesToFloat bs) else Nothing

instance Native Double where
    showBytes i bs = doubleToBytes i bs
    readBytes bs = if hasNElems lenDouble bs then Just (bytesToDouble bs) else Nothing

instance Native Bool where
    showBytes b bs = if b then '\x01':bs else '\x00':bs
    readBytes [] = Nothing
    readBytes (c:cs) = Just(c/='\x00', cs)

-- A pair is stored as two consectutive items.
instance (Native a, Native b) => Native (a,b) where
    showBytes (a,b) = showBytes a . showBytes b
    readBytes bs = readBytes bs  `thenM` \(a,bs') -> 
                   readBytes bs' `thenM` \(b,bs'') ->
                   Just ((a,b), bs'')

-- A triple is stored as three consectutive items.
instance (Native a, Native b, Native c) => Native (a,b,c) where
    showBytes (a,b,c) = showBytes a . showBytes b . showBytes c
    readBytes bs = readBytes bs  `thenM` \(a,bs') -> 
                   readBytes bs' `thenM` \(b,bs'') ->
                   readBytes bs'' `thenM` \(c,bs''') ->
                   Just ((a,b,c), bs''')

-- A list is stored with an Int with the number of items followed by the items.
instance (Native a) => Native [a] where
    showBytes xs bs = showBytes (length xs) (f xs) where f [] = bs
                                                         f (x:xs) = showBytes x (f xs)
    readBytes bs = readBytes bs `thenM` \(n,bs') ->
                   listReadBytes n bs' `thenM` \(xs, bs'') ->
                   Just (xs, bs'')


instance (Native a) => Native (Maybe a) where
    showBytes Nothing = ('\x00' :)
    showBytes (Just x) = ('\x01' :) . showBytes x
    readBytes ('\x00':bs) = Just (Nothing, bs)
    readBytes ('\x01':bs) = readBytes bs `thenM` \(a,bs') ->
                            Just (Just a, bs')
    readBytes _ = Nothing

instance (Native a, Ix a, Native b) => Native (Array a b) where
    showBytes a = showBytes (bounds a) . showBytes (elems a)
    readBytes bs = readBytes bs `thenM` \(b, bs')->
                   readBytes bs' `thenM` \(xs, bs'')->
		   Just (listArray b xs, bs'')

shortIntToBytes :: Int -> Bytes -> Bytes
shortIntToBytes s bs = shortToBytes s bs

bytesToShortInt :: Bytes -> Maybe (Int, Bytes)
bytesToShortInt bs = if hasNElems lenShort bs then Just (bytesToShort bs) else Nothing

longIntToBytes :: Int -> Bytes -> Bytes
longIntToBytes s bs = longToBytes s bs

bytesToLongInt :: Bytes -> Maybe (Int, Bytes)
bytesToLongInt bs = if hasNElems lenLong bs then Just (bytesToLong bs) else Nothing

showB :: (Native a) => a -> Bytes
showB x = showBytes x []

readB :: (Native a) => Bytes -> a
readB bs = 
	case readBytes bs of
	Just (x,[]) -> x
	Just (_,_)  -> error "Native.readB data to long"
        Nothing     -> error "Native.readB data to short"

