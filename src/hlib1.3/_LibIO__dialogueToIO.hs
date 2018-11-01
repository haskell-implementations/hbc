module _LibIO__dialogueToIO where

import _LibDialogue
import _LibIO

dialogueToIO :: _Dialogue -> IO ()
dialogueToIO d =
    IO $ \ uToD -> 
    \ resps ->
    let f [] resps = uToD (Right ()) resps
	f (req:reqs) resps = req : case resps of _ : resps' -> f reqs resps'
    in  f (d resps) resps

