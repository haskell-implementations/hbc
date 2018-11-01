module StdIO__hIsEOF where

import _LibDialogue
import _LibIO__process
import _LibIO(Handle(..), IO)
import StdIO__std

hIsEOF :: Handle -> IO Bool
hIsEOF h@(Handle _ f) = processRequestIOInt (Just h) Nothing (H_IsEOF f) >>= return . (/= 0)

isEOF :: IO Bool
isEOF = hIsEOF stdin

