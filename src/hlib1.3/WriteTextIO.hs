module WriteTextIO where
import MonadicIO
import IOError
import StdIO
import _LibIO__process
import _LibIO(Handle(..), IO)
import _LibDialogue

hPutChar      ::           Handle -> Char   -> IO ()
hPutChar h@(Handle _ f) c = processRequestIOUnit (Just h) Nothing (H_PutChar f (fromEnum c))

putChar       ::                     Char   -> IO () 
putChar       =  hPutChar stdout

hPutStr       ::           Handle -> String -> IO ()
hPutStr h@(Handle _ f) s = processRequestIOUnit (Just h) Nothing (H_PutString f s)

hPrint      :: Show a => Handle -> a      -> IO ()
hPrint hdl  =  hPutStr hdl . show

putStr        ::                     String -> IO () 
putStr        =  hPutStr stdout

putStrLn      ::                     String -> IO () 
putStrLn s    =  hPutStr stdout s >> putChar '\n'

print         :: Show a =>           a      -> IO ()
print x       =  putStr (show x) >> putChar '\n'

writeFile  :: FilePath -> String -> IO ()
writeFile name str =
    do
        hdl <- openFile name WriteMode
	hPutStr hdl str
	hClose hdl

appendFile :: FilePath -> String -> IO ()
appendFile name str =
    do
        hdl <- openFile name AppendMode
	hPutStr hdl str
	hClose hdl
