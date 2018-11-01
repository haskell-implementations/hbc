module StdIO__hClose where

import _LibDialogue
import _LibIO__process
import _LibIO(Handle(..), IO)

hClose  :: Handle -> IO ()
hClose h@(Handle _ f) = processRequestIOUnit (Just h) Nothing (H_Close f)

