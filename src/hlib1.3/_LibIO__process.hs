module _LibIO__process where

import LMLstrerror
import _LibDialogue
import _LibIO(IO(..), Handle, IOError(..), IOErrorCode(..))
import _LibIO__fail

processRequestIO :: _Request -> IO _Response
processRequestIO req = IO $ \ k ~(resp:resps) -> req : k (Right resp) resps

processRequestIOUnit :: Maybe Handle -> Maybe String -> _Request -> IO ()
processRequestIOUnit mh ms req =
    processRequestIO req >>= \ resp -> 
    case resp of
    Success       -> return ()
    Failure ioerr -> getIOErrorCode mh ms ioerr
    _             -> procError "Success" 

processRequestIOFile :: Maybe Handle -> Maybe String -> _Request -> IO _File
processRequestIOFile mh ms req =
    processRequestIO req >>= \ resp -> 
    case resp of
    Fil f         -> return f
    Failure ioerr -> getIOErrorCode mh ms ioerr
    _             -> procError "Fil" 

processRequestIOInt :: Maybe Handle -> Maybe String -> _Request -> IO Int
processRequestIOInt mh ms req =
    processRequestIO req >>= \ resp -> 
    case resp of
    IntResp i     -> return i
    Failure ioerr -> getIOErrorCode mh ms ioerr
    _             -> procError "IntResp" 

processRequestIOString :: Maybe Handle -> Maybe String -> _Request -> IO String
processRequestIOString mh ms req =
    processRequestIO req >>= \ resp -> 
    case resp of
    Str s         -> return s
    Failure ioerr -> getIOErrorCode mh ms ioerr
    _             -> procError "Str" 

processRequestIOStringList :: Maybe Handle -> Maybe String -> _Request -> IO [String]
processRequestIOStringList mh ms req =
    processRequestIO req >>= \ resp -> 
    case resp of
    StrList ss    -> return ss
    Failure ioerr -> getIOErrorCode mh ms ioerr
    _             -> procError "StrList" 

processRequestIODouble :: Maybe Handle -> Maybe String -> _Request -> IO Double
processRequestIODouble mh ms req =
    processRequestIO req >>= \ resp -> 
    case resp of
    Dbl d         -> return d
    Failure ioerr -> getIOErrorCode mh ms ioerr
    _             -> procError "Dbl" 

processRequestIOSigActResp :: Maybe Handle -> Maybe String -> _Request -> IO SigAct
processRequestIOSigActResp mh ms req =
    processRequestIO req >>= \ resp -> 
    case resp of
    SigActResp a  -> return a
    Failure ioerr -> getIOErrorCode mh ms ioerr
    _             -> procError "SigActResp" 

processRequestIOSelectData :: Maybe Handle -> Maybe String -> _Request -> IO [([_File], [_File], [Double])]
processRequestIOSelectData mh ms req =
    processRequestIO req >>= \ resp -> 
    case resp of
    SelectResp r  -> return r
    Failure ioerr -> getIOErrorCode mh ms ioerr
    _             -> procError "SelectResp" 

processRequestIO_CUnion :: Maybe Handle -> Maybe String -> _Request -> IO _CUnion
processRequestIO_CUnion mh ms req =
    processRequestIO req >>= \ resp -> 
    case resp of
    CCallResp r   -> return r
    Failure ioerr -> getIOErrorCode mh ms ioerr
    _             -> procError "CCallResp" 

processRequestIOGetTimeZone :: Maybe Handle -> Maybe String -> _Request -> IO (Bool, String, Int)
processRequestIOGetTimeZone mh ms req =
    processRequestIO req >>= \ resp -> 
    case resp of
    GetTimeZoneResp b s i -> return (b, s, i)
    Failure ioerr -> getIOErrorCode mh ms ioerr
    _             -> procError "GetTimeZoneResp"

procError :: String -> a
procError s = error ("Bad Response, expected a "++s)

getIOErrorCode :: Maybe Handle -> Maybe String -> D_IOError -> IO b
getIOErrorCode h s (OtherError "EOF") =
    fail (IOError EOF h s)
getIOErrorCode h s _ =
    processRequestIO H_GetErrno >>= \ (IntResp i) ->
    fail (IOError (posixError i) h s)
    
posixError i = PosixErrno i (strerror i)
