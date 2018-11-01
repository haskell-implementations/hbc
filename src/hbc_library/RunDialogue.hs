module RunDialog(run, runTrace) where
import IO
import Either
import Maybe

run :: Dialogue -> String
run d =
        let reqs = d resps
            (outs, resps) = unzip (map reply reqs)
	in  unlines outs

runTrace :: Dialogue -> String
runTrace d =
        let reqs = d resps
            (resps, outs) = unzip (map doOneTrace reqs)
	in  unlines outs

doOneTrace r = let (s, a) = reply r in (a, show r ++ " => " ++ show a ++ " " ++ s)

reply (ReadFile n) 		= ("", lookup n)
reply (WriteFile n c)		= (toFile n++c++toStdout, Success)
reply (AppendFile n c)		= (toFileAppend n++c++toStdout, Success)
reply (DeleteFile n)		= ("", Failure (OtherError "Delete unimpl"))
reply (StatusFile n)		= ("", stat n)

reply (AppendChan "stdout" c)	= (toStdout++c, Success)
reply (AppendChan "stderr" c)	= (toStderr++c++toStdout, Success)
reply (AppendChan "stdecho" c)	= (toStderr++c++toStdout, Success)
reply (StatusChan n)		= ("", if n `elem` ["stdout","stderr","stdecho"] then Str "" else Failure (OtherError ""))

reply (Echo b)			= (if b then setCooked else setCbreak, Success)
reply (GetArgs)			= ("", StrList [])
reply (GetEnv s)		= ("", getenv s)
reply (GetProgName)		= ("", Str progName)
reply _				= ("", Failure (OtherError "Unimpl"))

lookup n =
	case openFile n of
	Left msg -> Failure (SearchError msg)
	Right file -> Str file

getenv s =
	case getEnvi s of
	Nothing -> Failure (SearchError s)
	Just env -> Str env

stat s =
	case statFile s of
	Left msg -> Failure (SearchError msg)
	Right (_:_:mode:_:pid:gid:_) -> Str "frw"		-- wrong!!

