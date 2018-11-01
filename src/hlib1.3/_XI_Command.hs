module _XI_Command where

import _XStuff

instance Show XCommand where
    showsType _ = showString "XCommand"
instance Show XRequest where
    showsType _ = showString "XRequest"
