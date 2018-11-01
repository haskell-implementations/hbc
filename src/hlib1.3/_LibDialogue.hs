#define Bin ()
#define IOError D_IOError
#define File _File
#define Text Show
#define P_IO_data _LibDialogue
#define XStuff _XStuff
#define ISO(t, c, x)newtype t = c x
#define Request _Request
#define Response _Response
#define Dialogue _Dialogue
#define EXISTVAR ?a
#define IMPORT import _ByteVector

#include "../hlib/P_IO_data.hs"

instance Show Prelude._File where
    showsType _ = showString "_File"

type Dialogue = [Response] -> [Request]

