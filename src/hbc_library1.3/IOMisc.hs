module IOMisc where
import IO

hGetLine :: Handle -> IO String
hGetLine h =
        hGetChar h >>= \ c ->
	if c == '\n' then return "" else
	hGetLine h >>= \ cs ->
	return (c:cs)

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h s =
	hPutStr h s >>
	hPutChar h '\n'
