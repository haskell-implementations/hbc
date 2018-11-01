module MutArray(MutArray, newMutArray, readMutArray, writeMutArray, freezeMutArray, _unsafeFreezeMutArray) where
import State
import _LibArray
import Array

data (Ix i) => MutArray s i a = MA !(i,i) !(MutVector s a)

newMutArray :: (Ix i) => (i, i) -> a -> ST s (MutArray s i a)
newMutArray b@(l,u) x = newMutVector (index b u) x >>= return . MA b

readMutArray :: (Ix i) => MutArray s i a -> i -> ST s a
readMutArray (MA b v) i = if inRange b i then readMutVector v (index b i) else error "MutArray.readMutArray: index out of bounds"

writeMutArray :: (Ix i) => MutArray s i a -> i -> a -> ST s ()
writeMutArray (MA b v) i x = if inRange b i then writeMutVector v (index b i) x else error "MutArray.writeMutArray: index out of bounds"

bounds :: (Ix i) => MutArray s i a -> (i, i)
bounds (MA b _) = b

freezeMutArray :: (Ix i) => MutArray s i a -> ST s (Array i a)
freezeMutArray (MA b@(l,u) v) = do
    ixs <- mapM (\ i -> readMutVector v (index b i) >>= \ v -> return (i, v)) (range b)
    return (array b ixs)

_unsafeFreezeMutArray :: (Ix i) => MutArray s i a -> Array i a
_unsafeFreezeMutArray (MA b v) = MkArray b (_unsafeFreezeVector v)
