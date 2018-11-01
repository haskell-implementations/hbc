module PreludeX (
	IO, FilePath(..),
	putChar, putStr, putStrLn, print,
	getChar, getLine, getContents, interact,
	readFile, writeFile, appendFile, readIO, readLn,
	fail, userError, catch, IOError
    ) where

import IO

getLine :: IO String
getLine = hGetLine stdin

getContents :: IO String
getContents = hGetContents stdin

readIO :: (Read a) => String -> IO a
readIO s =
	case [x | (x,t) <- reads s, ("","") <- lex t] of
	[x] -> return x
	[]  -> fail (userError "Prelude.read: no parse")
	_   -> fail (userError "Prelude.read: ambiguous parse")

readLn :: (Read a) => IO a
readLn = getLine >>= readIO
