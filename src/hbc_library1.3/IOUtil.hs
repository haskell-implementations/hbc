module IOUtil(getEnvi, progName, progArgs) where
-- Some utilities that are a little dirty, but not very.
import IO
import UnsafePerformIO
import System

getEnvi :: String -> Maybe String
getEnvi s = hdl (getEnv s >>= (return . Just)) Nothing

progName :: String
progName = hdl getProgName "<<progName>>"

progArgs :: [String]
progArgs = hdl getArgs []

hdl :: IO a -> a -> a
hdl io def = unsafePerformIO (catch io (\err -> return def))
