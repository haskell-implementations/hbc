module Interrupt where
#if 0
import IO
import _LibIO__process
import _LibIO__ioToDialogue
import _LibIO__dialogueToIO
import _LibDialogue
#define SIGINT 2

setUserInterrupt :: Maybe (IO ()) -> IO (Maybe (IO ()))
setUserInterrupt f =
	processRequestIOSigActResp Nothing Nothing (SigAction SIGINT (mkSA f)) >>= \ a ->
	case a of
	SADefault -> return Nothing
	SAIgnore -> return Nothing
	SACatch sdialogue -> return (Just (dialogueToIO (sdialogue "")))
  where mkSA Nothing = SADefault
        mkSA (Just f) = SACatch (const (ioToDialogue f))

#else

import HandleException
setUserInterrupt :: Maybe (IO ()) -> IO (Maybe (IO ()))
setUserInterrupt f = do
	x <- handleException excInterrupt (case f of Nothing -> ExcDefault; Just io -> ExcHandle (const io)) 
	case x of
	    ExcHandle f -> return (Just (f ""))
	    _ -> return Nothing

#endif
