module PreludeX where

instance Text Descriptor where
    showsType _ = showString "Descriptor"
instance Text AEvent where
    showsType _ = showString "AEvent"
instance Text SocketRequest where
    showsType _ = showString "SocketRequest"
instance Text SocketResponse where
    showsType _ = showString "SocketResponse"
instance Text WindowId where
    showsType _ = showString "WindowId"
instance Text XCommand where
    showsType _ = showString "XCommand"
instance Text XEvent where
    showsType _ = showString "XEvent"
instance Text XRequest where
    showsType _ = showString "XRequest"
instance Text XResponse where
    showsType _ = showString "XResponse"
instance Text Display where
    showsType _ = showString "Display"

