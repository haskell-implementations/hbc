module DialogueIO(Dialogue(..), Request(..), Response(..), IOError(..), dialogueToIO, module _LibDialogue) where
import Prelude hiding (IOError)
import _LibIO_all
import _LibDialogue

--@@ Haskell 1.2 dialugue I/O compatibility.
--@NOINTERFACE
type Dialogue = _Dialogue
type Request = _Request
type Response = _Response
type IOError = D_IOError
