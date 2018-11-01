module _LibIO where

import _LibDialogue
import _RunDialogue
import LMLunsafe

type IOT a = (a -> _Dialogue) -> _Dialogue

newtype IO a = IO (IOT (Either IOError a))

data Handle = Handle IOMode _File

data IOMode     = ReadMode
                | WriteMode
                | AppendMode 
                | ReadWriteMode
		deriving (Eq, Ord, Show, Read, Enum, Ix, Bounded)

instance Show Handle where
    showsType _ = showString "Handle"

instance Eq Handle where
    (Handle _ f) == (Handle _ f')  =  {-:"Peqptr":-} f f'

data IOErrorCode 
	= EOF
	| User String
	| PosixErrno Int String
	deriving (Eq)

data IOError = IOError IOErrorCode (Maybe Handle) (Maybe String) deriving (Eq)
