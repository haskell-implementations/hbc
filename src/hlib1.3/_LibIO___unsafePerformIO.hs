module _LibIO___unsafePerformIO where

import _LibDialogue
import _RunDialogue
import _LibIO
import _LibIO__mkErrMsg

-- If this isn't magic, then what is? :-)
_unsafePerformIO :: IO a -> a
_unsafePerformIO (IO dfun) =
    let rfun (Left err) = error ("_unsafePerformIO error: "++ mkErrMsg err)
        rfun (Right x) = \ resps -> [__RunAnswer (__Answer x)]
    in  __runDialogue (dfun rfun)
