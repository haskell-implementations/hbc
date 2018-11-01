module System where
import IO
import _LibIO__process
import _LibDialogue
import _LibIO

-- XXX
#define eSRCH 3

data ExitCode = ExitSuccess | ExitFailure Int deriving (Eq, Ord, Show, Read)

getArgs :: IO [String] 
getArgs = processRequestIOStringList Nothing Nothing GetArgs

getProgName :: IO String
getProgName = processRequestIOString Nothing Nothing GetProgName

getEnv        :: String -> IO String
getEnv var = 
	processRequestIOString Nothing Nothing (GetEnv var)
        `catch`
        \ _ -> fail (IOError (posixError eSRCH)  Nothing Nothing)

system        :: String -> IO ExitCode
system cmd = (processRequestIOUnit Nothing Nothing (System cmd) >> return ExitSuccess) `catch` \ _ -> return (ExitFailure 1) -- XXX should decode exit code

exitWith      :: ExitCode -> IO a
exitWith e = processRequestIOUnit Nothing Nothing (Exit (case e of ExitSuccess -> 0; ExitFailure n -> n)) >> return (error "System.exitWith did not exit!")
