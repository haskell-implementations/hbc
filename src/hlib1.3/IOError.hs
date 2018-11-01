module IOError(IOError, isAlreadyExistsError, isAlreadyInUseError, isFullError,
	isEOFError, isIllegalOperation, isPermissionError, isDoesNotExistError, 
	isUserError, ioeGetHandle, ioeGetFileName, ioeGetErrorString) where

import _LibDialogue
import _LibIO_all
import _LibIOErrno

isEOFError :: IOError -> Bool
isEOFError (IOError EOF _ _) = True
isEOFError _ = False

isUserError :: IOError -> Maybe String
isUserError (IOError (User s) _ _) = Just s
isUserError _ = Nothing

ioeGetHandle :: IOError -> Maybe Handle
ioeGetHandle (IOError _ h _) = h

ioeGetFileName :: IOError -> Maybe FilePath
ioeGetFileName (IOError _ _ n) = n

ioeGetErrorString :: IOError -> Maybe String
ioeGetErrorString (IOError (PosixErrno _ msg) _ _) = Just msg
ioeGetErrorString _ = Nothing
