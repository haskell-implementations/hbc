module ReadTextIO where
import MonadicIO
import StdIO
import _LibIO__process
import _LibIO(Handle(..), IO)
import _LibDialogue

hReady        :: Handle -> IO Bool
hReady h = hWaitForInput 0 h

hWaitForInput :: Int -> Handle -> IO Bool
hWaitForInput t h@(Handle _ f) =
	processRequestIOSelectData (Just h) Nothing (H_Select ([f], [], [fromInt t / 1000.0])) >>= \ r -> 
	case r of
	[([_], _, _)] -> return True
	_ -> return False

hGetChar      :: Handle -> IO Char
hGetChar h@(Handle _ f) = processRequestIOInt (Just h) Nothing (H_GetChar f) >>= return . toEnum

getChar       ::           IO Char
getChar       =  hGetChar stdin

hLookAhead    :: Handle -> IO Char
hLookAhead h@(Handle _ f) = 
	processRequestIOInt (Just h) Nothing (H_GetChar f) >>= \ c ->
	processRequestIOUnit (Just h) Nothing (H_UnGetChar f c) >>
	return (toEnum c)

hGetLine      :: Handle -> IO String
hGetLine h =
        hGetChar h >>= \ c ->
	if c == '\n' then return "" else
	hGetLine h >>= \ cs ->
	return (c:cs)

hGetContents  :: Handle -> IO String
hGetContents h@(Handle _ f) = processRequestIOString (Just h) Nothing (H_GetFile f)

readFile      :: FilePath -> IO String
readFile name =  openFile name ReadMode >>= hGetContents

