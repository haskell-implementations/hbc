module _XI_Sockets where

import _XStuff

instance Show Descriptor where
    showsType _ = showString "Descriptor"
instance Show AEvent where
    showsType _ = showString "AEvent"
instance Show SocketRequest where
    showsType _ = showString "SocketRequest"
instance Show SocketResponse where
    showsType _ = showString "SocketResponse"

