module _LibIO__mkErrMsg(mkErrMsg, mkMessageCode, IOError) where

import _LibIO(IOError(..), IOErrorCode(..))

mkErrMsg (IOError code _ s) = mkMessageCode code s
mkMessageCode EOF _ = "End-Of-File"
mkMessageCode (User s) _ = ("User error: " ++ s)
mkMessageCode (PosixErrno n msg) s = 
	msg ++" (errno="++show n++")"++case s of Just f -> ", `"++ f ++ "'" ; _ -> ""


instance Show IOError where
    showsType _ = showString "IOError"
    showsPrec _ err = showString ("I/O error: "++ mkErrMsg err)
