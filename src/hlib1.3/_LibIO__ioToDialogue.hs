module _LibIO__ioToDialogue where
import _LibDialogue
import _LibIO(IO(..))
import _LibIO__mkErrMsg

ioToDialogue :: IO a -> _Dialogue
ioToDialogue (IO m) = 
    m (\ ex ->
	case ex of
	Left e -> error (mkErrorMessage e)
	Right _ -> \ _ -> [])

mkErrorMessage err = "Program error: " ++ mkErrMsg err

-- Used by the interactive system.
_ioToDialogue :: IO a -> _Dialogue
_ioToDialogue io = ioToDialogue io
