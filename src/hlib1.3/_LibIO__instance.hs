module _LibIO__instance where

import _LibIO

instance Functor IO where
    map f (IO m) = IO $ \ k ->
        m (\ ex -> 
	    case ex of
	    Left e -> k (Left e)
	    Right x -> k (Right (f x)))

instance Monad IO where
    return x = IO $ \ k -> k (Right x)
    IO m >>= f = IO $ \ k ->
        m (\ ex -> 
	    case ex of
	    Left e -> k (Left e)
	    Right x -> case f x of IO y -> y k)
    IO m >> IO y = IO $ \ k ->
        m (\ ex -> 
	    case ex of
	    Left e -> k (Left e)
	    Right _ -> y k)


