module _BV_Inst where
import _ByteVector

instance Eq _ByteVector
instance Ord _ByteVector
instance Show _ByteVector where
    showsType _ = showString "_ByteVector"
