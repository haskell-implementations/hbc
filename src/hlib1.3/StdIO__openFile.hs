module StdIO__openFile where

#include "../lib/dialogdef.h"

import _LibDialogue
import _LibIO__process
import _LibIO(Handle(..), IO, IOMode(..))

openFile :: FilePath -> IOMode -> IO Handle
openFile name mode = processRequestIOFile Nothing (Just name) (H_OpenFile name (modeToInt mode)) >>= return . Handle mode

openBinaryFile :: FilePath -> IOMode -> IO Handle
openBinaryFile name mode = processRequestIOFile Nothing (Just name) (H_OpenFile name (modeToInt mode + BINARYMODE)) >>= return . Handle mode

modeToInt :: IOMode -> Int
modeToInt ReadMode = READMODE
modeToInt WriteMode = WRITEMODE
modeToInt AppendMode = APPENDMODE
modeToInt ReadWriteMode = READWRITEMODE

