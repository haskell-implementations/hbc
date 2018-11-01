module HandleException where
import IO
import _LibIO__process
import _LibIO__ioToDialogue
import _LibIO__dialogueToIO
import _LibDialogue

data ExceptionHandler
	= ExcDefault
	| ExcIgnore
	| ExcHandle (String -> IO ())

handleException :: Exception -> ExceptionHandler -> IO ExceptionHandler
handleException n f =
	processRequestIOSigActResp Nothing Nothing (SigAction n (mkSA f)) >>= return . mkExc
  where mkSA ExcDefault    = SADefault
        mkSA ExcIgnore     = SAIgnore
        mkSA (ExcHandle f) = SACatch (ioToDialogue . f)
	mkExc SADefault    = ExcDefault
	mkExc SAIgnore     = ExcIgnore
	mkExc (SACatch f)  = ExcHandle (dialogueToIO . f)

type Exception = Int
excError, excInterrupt, excTerminate, excHangup, excPipe, excArithmetic :: Exception
excError     = 0
excHangup    = 1
excInterrupt = 2
excArithmetic= 8
excPipe      = 13
excTerminate = 15
