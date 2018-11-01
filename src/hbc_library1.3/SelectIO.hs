module SelectIO where
import MonadicIO
import StdIO
import _LibIO__process
import _LibIO(Handle(..), IO)
import _LibDialogue

type SelectData = ([Handle], [Handle], Maybe Double)

select :: SelectData -> IO (Maybe SelectData)
select (is, os, ot) = 
	processRequestIOSelectData Nothing Nothing (H_Select (unHandleL is, unHandleL os, conv ot)) >>= \ r ->
	case r of
	[(is', os', ot')] -> return (Just (map (Handle ReadMode) is', map (Handle WriteMode) os', rconv ot')) -- XX handles are not done right
	_ -> return Nothing
  where unHandleL (Handle _ f : hs) = f : unHandleL hs  -- have to be careful not to evaluate f
        unHandleL [] = []
        conv :: Maybe Double -> [Double]
	conv Nothing = []
	conv (Just d) = [d]
	rconv :: [Double] -> Maybe Double
	rconv [] = Nothing
	rconv [d] = Just d
