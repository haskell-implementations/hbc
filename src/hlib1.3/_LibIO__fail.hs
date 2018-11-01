module _LibIO__fail where

import _LibIO(IO(..), IOError)

fail :: IOError -> IO a
fail e = IO $ \ k -> k (Left e)

