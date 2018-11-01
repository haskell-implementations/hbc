module MonadicIO (
	IO, catch, userError, 
        bracket, bracket_,
	try, fail) where

import _LibDialogue
import _LibIO

catch :: IO a -> (IOError -> IO a) -> IO a 
catch (IO m) h = IO $ \ k ->
    m (\ ex -> 
	case ex of
	Left e -> let IO y = h e in y k
	Right x -> k ex)

userError :: String -> IOError
userError s = IOError (User s) Nothing Nothing

bracket        :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after m = do
        x  <- before
        rs <- try (m x)
        after x
        case rs of
           Right r -> return r
           Left  e -> fail e

-- variant of the above where middle computation doesn't want x
bracket_        :: IO a -> (a -> IO b) -> IO c -> IO c
bracket_ before after m = do
         x  <- before
         rs <- try m
         after x
         case rs of
            Right r -> return r
            Left  e -> fail e

try :: IO a -> IO (Either IOError a)
try (IO m) = IO $ \ k ->
    m (\ ex -> 
	case ex of
	Left e -> k (Left e)
	Right x -> k (Right ex))

