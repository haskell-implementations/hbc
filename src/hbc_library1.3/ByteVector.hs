module ByteVector(
	ByteVector, Byte, 
	append, sub, empty, fromList, substr, toList, size,
	cPointerToByteVector,
	getNet16, getNet32, setNet16, setNet32
	) where
import _ByteVector
import CCall

newtype Byte = B Char deriving (Eq, Ord)

instance Show Byte where
    showsType _ = showString "Byte"
    showsPrec _ (B c) = showString [hex!!h, hex!!l]
        where hex = "0123456789ABCDEF"
	      i = fromEnum c
	      h = i `div` 16
	      l = i `mod` 16

instance Enum Byte where
    fromEnum (B c) = fromEnum c
    toEnum i = B (toEnum i)

instance Bounded Byte where
    minBound = B '\x00'
    maxBound = B '\xff'

newtype ByteVector = BV _ByteVector

instance Eq ByteVector where
    (BV p1) == (BV p2) = compareBV p1 p2 == 0
    (BV p1) /= (BV p2) = compareBV p1 p2 /= 0

instance Ord ByteVector where
    (BV p1) <  (BV p2) = compareBV p1 p2 <  0
    (BV p1) <= (BV p2) = compareBV p1 p2 <= 0
    (BV p1) >  (BV p2) = compareBV p1 p2 >  0
    (BV p1) >= (BV p2) = compareBV p1 p2 >= 0

instance Show ByteVector where
    showsType _ = showString "ByteVector"

instance CValue ByteVector where
    toCU (BV x) = toCU x
    fromCU x = BV (fromCU x)

append (BV p1) (BV p2) = BV (appendBV p1 p2)

-- index p i
-- returns the Byte at position i (0 based)
sub :: ByteVector -> Int -> Byte
BV p `sub` i = B (indexBV p i)

empty :: ByteVector
empty = BV nilBV

fromList :: [Byte] -> ByteVector
fromList bs = BV (packBV [ c | B c <- bs ])

toList :: ByteVector -> [Byte]
toList (BV p) = map B (unpackBV p)

substr :: ByteVector -> Int -> Int -> ByteVector
substr (BV p) l h = BV (substrBV p l h)

cPointerToByteVector :: _CPointer -> Int -> ByteVector
cPointerToByteVector p i = BV (cPointerToBV p i)

size :: ByteVector -> Int
size (BV p) = lengthBV p

getNet16 :: ByteVector -> Int
getNet16 (BV p) = fromEnum (indexBV p 0) * 0x100 + fromEnum (indexBV p 1)

getNet32 :: ByteVector -> Int
getNet32 (BV p) = 
	fromEnum (indexBV p 0) * 0x1000000 + 
	fromEnum (indexBV p 1) * 0x10000 +
	fromEnum (indexBV p 2) * 0x100 +
	fromEnum (indexBV p 3)

setNet16 :: Int -> ByteVector
setNet16 i = fromList [toEnum (i `div` 0x100), toEnum (i `mod` 0x100)]

setNet32 :: Int -> ByteVector
setNet32 i = fromList [toEnum (i `div` 0x1000000), toEnum ((i `div` 0x10000) `mod` 0x100), toEnum ((i `div` 0x100) `mod` 0x100), toEnum (i `mod` 0x100)]
