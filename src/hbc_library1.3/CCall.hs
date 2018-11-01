module CCall( module CCall, _CUnion, _CPointer ) where
import IO
import _LibIO__process
import _LibDialogue
import _ByteVector

type CUnion = _CUnion
type CPointer = _CPointer

ccallCU :: CPointer -> [CUnion] -> CUnion -> IO CUnion
ccallCU fun args ret = processRequestIO_CUnion Nothing Nothing (H_CCall fun args ret)

ccallCV :: (CValue a) => CPointer -> [CUnion] -> IO a
ccallCV fun args =
    let f = toCU
    in  ccallCU fun args (f (error "ccallCV")) >>= \ ux ->
        let x = fromCU ux
	    y = f x
	in  return x

nullCPointer :: CPointer
nullCPointer = _CPointer 0

addCPointer :: CPointer -> Int -> CPointer
addCPointer (_CPointer p) i = _CPointer (p+i)

class CValue a where
    toCU :: a -> CUnion
    fromCU :: CUnion -> a

instance CValue Int where
    toCU x = _CUInt x
    fromCU (_CUInt x) = x

instance CValue _CPointer where
    toCU x = _CUPointer x
    fromCU (_CUPointer x) = x

instance CValue Double where
    toCU x = _CUDouble x
    fromCU (_CUDouble x) = x

instance CValue Float where
    toCU x = _CUDouble (fromRealFrac x)
    fromCU (_CUDouble x) = fromRealFrac x

instance CValue Bool where
    toCU x = _CUInt (if x then 1 else 0)
    fromCU (_CUInt x) = x /= 0

instance CValue () where
    toCU x = _CUInt 0
    fromCU _ = ()

instance CValue Char where
    toCU x = _CUInt (fromEnum x)
    fromCU (_CUInt x) = toEnum x

instance CValue [a]	-- dummy to allow the definition below

instance CValue [Char] where
    toCU x = _CUString x
    fromCU (_CUString x) = x

instance CValue _ByteVector where
    toCU x = _CUByteVector x
    fromCU (_CUByteVector x) = x
