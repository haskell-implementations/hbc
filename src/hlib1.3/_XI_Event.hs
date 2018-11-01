module _XI_Event where

import _XStuff

instance Show XEvent where
    showsType _ = showString "XEvent"
instance Show XResponse where
    showsType _ = showString "XResponse"
