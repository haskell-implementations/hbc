module StdIO__data where

type FilePath = String

data BufferMode = NoBuffering
                | LineBuffering
                | BlockBuffering (Maybe Int)
		deriving (Eq, Ord, Show, Read)

data HandlePosn = HandlePosn Int
		deriving (Eq, Ord)

instance Show HandlePosn where
    showsType _ = showString "HandlePosn"


data SeekMode   = AbsoluteSeek
                | RelativeSeek
                | SeekFromEnd 
		deriving (Eq, Ord, Show, Read, Enum, Ix, Bounded)


