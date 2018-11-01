module StdIO(
	Handle, FilePath(..), IOMode(..), BufferMode(..),
	HandlePosn, SeekMode(..),
	stdin, stdout, stderr,
	openFile, openBinaryFile, hClose, hFileSize, hIsEOF,
	hSetBuffering, hFlush,
	hGetPosn, hSetPosn,
	hSeek,
	--hIsBlockBuffered, hIsLineBuffered, hIsNotBuffered,
	hIsOpen, hIsClosed, hIsReadable, hIsWritable, hIsSeekable
	) where
#include "../lib/dialogdef.h"
import MonadicIO
import _LibIO_all
import _LibDialogue
import StdIO__std
import StdIO__data
import StdIO__openFile
import StdIO__hClose
import StdIO__hIsEOF

hFileSize  :: Handle -> IO Integer
hFileSize h@(Handle _ f) = processRequestIOInt (Just h) Nothing (H_FileSize f) >>= return . toInteger

hSetBuffering :: Handle -> BufferMode  -> IO ()
hSetBuffering h@(Handle _ f) b = processRequestIOUnit (Just h) Nothing (H_SetBuffering f (bmToInt b))

hGetBuffering :: Handle -> IO BufferMode
hGetBuffering h@(Handle _ f) = processRequestIOInt (Just h) Nothing (H_GetBuffering f) >>= return . intToBm

hFlush  :: Handle -> IO ()
hFlush h@(Handle _ f) = processRequestIOUnit (Just h) Nothing (H_Flush f)

hGetPosn :: Handle -> IO HandlePosn
hGetPosn h@(Handle _ f) = processRequestIOInt (Just h) Nothing (H_Seek f 0 (smToInt RelativeSeek)) >>= return . HandlePosn

hSetPosn :: Handle -> HandlePosn -> IO ()
hSetPosn h@(Handle _ f) (HandlePosn i) = processRequestIOInt (Just h) Nothing (H_Seek f i (smToInt AbsoluteSeek)) >> return ()

hSeek :: Handle -> SeekMode -> Integer -> IO ()
hSeek h@(Handle _ f) sm i = processRequestIOInt (Just h) Nothing (H_Seek f (fromInteger i) (smToInt sm)) >> return ()

hIsOpen :: Handle -> IO Bool
hIsOpen h = flag h F_ISOPEN

hIsClosed :: Handle -> IO Bool
hIsClosed h = flag h F_ISCLOSED

hIsReadable :: Handle -> IO Bool
hIsReadable (Handle m _) = return (m == ReadMode || m == ReadWriteMode)

hIsWritable :: Handle -> IO Bool
hIsWritable (Handle m _) = return (m /= ReadMode)

hIsSeekable :: Handle -> IO Bool
hIsSeekable h = flag h F_ISSEEKABLE

{-
hIsBlockBuffered :: Handle -> IO (Bool,Maybe Int)
hIsBlockBuffered h = 
	hGetBuffering h >>= \ b ->
	case b of
	BlockBuffering m -> return (True, m)
	_ -> return (False, Nothing)
hIsLineBuffered  :: Handle -> IO Bool
hIsLineBuffered h =
	hGetBuffering h >>= \ b ->
	case b of
	LineBuffering -> return True
	_ -> return False
hIsNotBuffered   :: Handle -> IO Bool
hIsNotBuffered h =
	hGetBuffering h >>= \ b ->
	case b of
	NoBuffering -> return True
	_ -> return False
-}

---
smToInt :: SeekMode -> Int
smToInt AbsoluteSeek = ABSOLUTESEEK
smToInt RelativeSeek = RELATIVESEEK
smToInt SeekFromEnd = SEEKFROMEND

bmToInt :: BufferMode -> Int
bmToInt NoBuffering = NOBUFFERING
bmToInt LineBuffering = LINEBUFFERING
bmToInt (BlockBuffering Nothing) = BLOCKBUFFERING
bmToInt (BlockBuffering (Just i)) = -i

intToBm :: Int -> BufferMode
intToBm i = 
	if i < 0 then BlockBuffering (Just (-i))
	else if i == NOBUFFERING then NoBuffering
	else if i == LINEBUFFERING then LineBuffering
	else BlockBuffering Nothing

flag :: Handle -> Int -> IO Bool
flag hh@(Handle _ h) f = processRequestIOInt (Just hh) Nothing (H_GetFlags h) >>= \ i -> return (odd (i `div` 2^f))
