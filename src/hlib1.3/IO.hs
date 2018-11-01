module IO (
      module MonadicIO, 
      module StdIO,
      module ReadTextIO,
      module WriteTextIO,
      module IOError,
      interact
    ) where
import MonadicIO
import StdIO
import IOError
import ReadTextIO
import WriteTextIO

interact :: (String -> String) -> IO ()
interact f = hGetContents stdin >>= putStr . f
