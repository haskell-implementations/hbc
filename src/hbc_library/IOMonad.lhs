Simulation of Glasgow Haskell I/O monad.
Written by Andy Gill <andy@dcs.gla.ac.uk>.

> module IOMonad (
>       IO(..),                 -- I would rather IO was abstract 
>       IoResult(..),
>	IOE(..),

>       returnIO, thenIO, thenIO_,
>       ioToDialogue,

>       readFileIOE, writeFileIOE, appendFileIOE, deleteFileIOE,
>       statusFileIOE, readChanIOE, appendChanIOE,statusChanIOE,
>       echoIOE, getArgsIOE, getEnvIOE, setEnvIOE,

>       readFileIO, writeFileIO, appendFileIO, deleteFileIO,
>       statusFileIO, readChanIO, appendChanIO, statusChanIO,
>       echoIO, getArgsIO, getEnvIO, setEnvIO,

>       exitIO, printIO, printsIO, interactIO, runIO, doneIO  ) where

%------------------------------------------------------------------------------

> infixr 9 `thenIO`, `thenIO_`

Here is a simulation of the IO monadary, as use at Glasgow and Yale.

> type IO a = (a -> Dialogue) -> Dialogue
>
> returnIO :: x -> IO x
> returnIO x cont = cont x

> thenIO :: IO a -> (a -> IO b) -> IO b
> thenIO m k cont = m (\ a -> k a cont)

> thenIO_ :: IO a -> IO b -> IO b
> thenIO_ m k cont = m (\ _ -> k cont)

> ioToDialogue :: IO a -> Dialogue
> ioToDialogue io = io (\ _ _ -> [])

> processRequestIO   :: Request -> IO Response
> processRequestIO req cont ~(resp:resps) = req : cont resp resps

> doneIO :: IO a
> doneIO cont = \ _ -> []

%------------------------------------------------------------------------------

> data IoResult a = IoSucc a
>                 | IoFail IOError

> type IOE a = IO (IoResult a)         

Our universal IO hook is processRequestIO.
These 3 utilities should really be hidden from the outside.

> processRequestIOUnit :: Request -> IOE ()
> processRequestIOUnit req =
>         processRequestIO req                           `thenIO` \ resp -> 
>         case resp of
>           Success       -> returnIO (IoSucc ())
>           Str str       -> error "funny Response, expected a Success"
>           StrList strl  -> error "funny Response, expected a Success" 
>           Failure ioerr -> returnIO (IoFail ioerr)
> 
> processRequestIOString :: Request -> IOE String
> processRequestIOString req =
>         processRequestIO req                           `thenIO` \ resp -> 
>         case resp of
>           Success       -> error "funny Response, expected a String"
>           Str str       -> returnIO (IoSucc str)
>           StrList strl  -> error "funny Response, expected a String" 
>           Failure ioerr -> returnIO (IoFail ioerr)
> 
> processRequestIOStringList :: Request -> IOE [String]
> processRequestIOStringList req =
>         processRequestIO req                           `thenIO` \ resp -> 
>         case resp of
>           Success       -> error "funny Response, expected a [String]"
>           Str str       -> error "funny Response, expected a [String]" 
>           StrList strl  -> returnIO (IoSucc strl)
>           Failure ioerr -> returnIO (IoFail ioerr)

%------------------------------------------------------------------------------

The functions that return Errors for the *user* to catch.

> readFileIOE     :: String ->           IOE String
> writeFileIOE    :: String -> String -> IOE ()
> appendFileIOE   :: String -> String -> IOE ()
> deleteFileIOE   :: String ->           IOE ()
> statusFileIOE   :: String ->           IOE String
> readChanIOE     :: String ->           IOE String
> appendChanIOE   :: String -> String -> IOE ()
> statusChanIOE   :: String ->           IOE String
> echoIOE         :: Bool   ->           IOE ()
> getArgsIOE      ::                     IOE [String]
> getEnvIOE       :: String ->           IOE String
> setEnvIOE       :: String -> String -> IOE ()

> readFileIOE    file     = processRequestIOString     ( ReadFile file )
> writeFileIOE   file str = processRequestIOUnit       ( WriteFile file str )
> appendFileIOE  file str = processRequestIOUnit       ( AppendFile file str )
> deleteFileIOE  file     = processRequestIOUnit       ( DeleteFile file )
> statusFileIOE  file     = processRequestIOString     ( StatusFile file )
> readChanIOE    chan     = processRequestIOString     ( ReadChan chan )
> appendChanIOE  chan str = processRequestIOUnit       ( AppendChan chan str )
> statusChanIOE  chan     = processRequestIOString     ( StatusChan chan )
> echoIOE        bool     = processRequestIOUnit       ( Echo bool )
> getArgsIOE              = processRequestIOStringList ( GetArgs )
> getEnvIOE      var      = processRequestIOString     ( GetEnv var )
> setEnvIOE      var obj  = processRequestIOUnit       ( SetEnv var obj )

%------------------------------------------------------------------------------

Next the easier  ``system catches errors'' versions.

> handleErrIO :: IoResult a -> IO a 
> handleErrIO (IoSucc a)     = returnIO a
> handleErrIO (IoFail ioerr) = exitIO   ioerr

> readFileIO      :: String ->           IO String
> writeFileIO     :: String -> String -> IO ()
> appendFileIO    :: String -> String -> IO ()
> deleteFileIO    :: String ->           IO ()
> statusFileIO    :: String ->           IO String
> readChanIO      :: String ->           IO String
> appendChanIO    :: String -> String -> IO ()
> statusChanIO    :: String ->           IO String
> echoIO          :: Bool   ->           IO ()
> getArgsIO       ::                     IO [String]
> getEnvIO        :: String ->           IO String
> setEnvIO        :: String -> String -> IO ()

> readFileIO      file       = readFileIOE file           `thenIO` handleErrIO
> writeFileIO     file str   = writeFileIOE file str      `thenIO` handleErrIO
> appendFileIO    file str   = appendFileIOE file str     `thenIO` handleErrIO
> deleteFileIO    file       = deleteFileIOE file         `thenIO` handleErrIO
> statusFileIO    file       = statusFileIOE file         `thenIO` handleErrIO
> readChanIO      chan       = readChanIOE chan           `thenIO` handleErrIO
> appendChanIO    chan str   = appendChanIOE chan str     `thenIO` handleErrIO
> statusChanIO    chan       = statusChanIOE chan         `thenIO` handleErrIO
> echoIO          bool       = echoIOE bool               `thenIO` handleErrIO
> getArgsIO                  = getArgsIOE                 `thenIO` handleErrIO
> getEnvIO        var        = getEnvIOE var              `thenIO` handleErrIO
> setEnvIO        var obj    = setEnvIOE var obj          `thenIO` handleErrIO

%------------------------------------------------------------------------------

> exitIO     :: IOError -> IO a
> printIO    :: Text a => a -> IO ()
> printsIO   :: Text a => a -> String -> IO ()
> interactIO :: (String -> String) -> IO ()
> runIO      :: (String -> String) -> IO ()

> exitIO (ReadError s)   = error s
> exitIO (WriteError s)  = error s
> exitIO (SearchError s) = error s
> exitIO (FormatError s) = error s
> exitIO (OtherError s)  = error s

> printIO x = appendChanIO stdout (show x)
> printsIO x s = appendChanIO stdout (shows x s)
> interactIO f = 
>       readChanIO stdin                `thenIO` \ x ->
>       appendChanIO stdout (f x)
> runIO f = echoIO False `thenIO_` interactIO f
