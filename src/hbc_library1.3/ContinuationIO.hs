#define P_IOC ContinuationIO(module ContinuationIO, module DialogueIO)
#define Text Show
#define IMPORTS import DialogueIO; import Prelude hiding (IOError)
#include "../hlib/P_IOC.hs"

--@@ Haskell 1.2 continuation I/O compatibility.
--@NOINTERFACE
type SuccCont    =                Dialogue
type StrCont     =  String     -> Dialogue
type BinCont     =  Bin        -> Dialogue
type FailCont    =  IOError    -> Dialogue
type StrListCont =  [String]   -> Dialogue
type Bin = ()

stdin = "stdin"
stdout = "stdout"
stderr = "stderr"
