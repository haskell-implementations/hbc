module -- from P_IOC
-- Continuation-based I/O:

#include "../lib/dialog.t"

rec
    type SuccCont    ==                Dialog
and type StrCont     ==  String     -> Dialog
and type BinCont     ==  Bin        -> Dialog
and type FailCont    ==  IOError    -> Dialog
and type StrListCont ==  [String]   -> Dialog

and done          ::                                             Dialog
and readFile      :: String ->           FailCont -> StrCont  -> Dialog
and writeFile     :: String -> String -> FailCont -> SuccCont -> Dialog
and appendFile    :: String -> String -> FailCont -> SuccCont -> Dialog

and readBinFile   :: String ->           FailCont -> BinCont  -> Dialog
and writeBinFile  :: String -> Bin    -> FailCont -> SuccCont -> Dialog
and appendBinFile :: String -> Bin    -> FailCont -> SuccCont -> Dialog
and deleteFile    :: String ->           FailCont -> SuccCont -> Dialog
and statusFile    :: String ->           FailCont -> StrCont  -> Dialog
and readChan      :: String ->           FailCont -> StrCont  -> Dialog
and appendChan    :: String -> String -> FailCont -> SuccCont -> Dialog
and readBinChan   :: String ->           FailCont -> BinCont  -> Dialog
and appendBinChan :: String -> Bin    -> FailCont -> SuccCont -> Dialog
and echo          :: Bool   ->           FailCont -> SuccCont -> Dialog
and getArgs       ::                     FailCont -> StrListCont -> Dialog
and getEnv        :: String ->           FailCont -> StrCont  -> Dialog
and setEnv        :: String -> String -> FailCont -> SuccCont -> Dialog
and getProgName   ::                     FailCont -> StrCont  -> Dialog
and readFileScattered :: String -> [Int] -> FailCont -> StrListCont  -> Dialog
and sigAction :: Exception -> SigAct -> FailCont -> (SigAct -> Dialog) -> Dialog
and system        :: String ->           FailCont -> SuccCont -> Dialog

and done resps    =  []

and readFile name fail succ resps =
     ReadFile name . strDispatch fail succ resps

and readFileScattered name offs fail succ resps =
     ReadFileScattered name offs . strListDispatch fail succ resps

and writeFile name contents fail succ resps =
    WriteFile name contents . succDispatch fail succ resps

and appendFile name contents fail succ resps =
    AppendFile name contents . succDispatch fail succ resps

and readBinFile name fail succ resps =
    ReadBinFile name . binDispatch fail succ resps

and writeBinFile name contents fail succ resps =
    WriteBinFile name contents . succDispatch fail succ resps

and appendBinFile name contents fail succ resps =
    AppendBinFile name contents . succDispatch fail succ resps

and deleteFile name fail succ resps =
    DeleteFile name . succDispatch fail succ resps

and statusFile name fail succ resps =
    StatusFile name . strDispatch fail succ resps

and readChan name fail succ resps =
    ReadChan name . strDispatch fail succ resps

and appendChan name contents fail succ resps =
    AppendChan name contents . succDispatch fail succ resps

and readBinChan name fail succ resps =
    ReadBinChan name . binDispatch fail succ resps

and appendBinChan name contents fail succ resps =
    AppendBinChan name contents . succDispatch fail succ resps

and echo bool fail succ resps =
    Echo bool . succDispatch fail succ resps

and getArgs fail succ resps =
    GetArgs . strListDispatch fail succ resps

and getProgName fail succ resps =
    GetProgName . strDispatch fail succ resps

and getEnv name fail succ resps =
    GetEnv name . strDispatch fail succ resps

and setEnv name val fail succ resps =
    SetEnv name val . succDispatch fail succ resps

and sigAction sig act fail succ resps =
    SigAction sig act . sigActDispatch fail succ resps


and system cmd fail succ resps =
    System cmd . succDispatch fail succ resps

and strDispatch  fail succ (resp.resps) = case resp in 
					     Str val      : succ val resps
					  || Failure msg  : fail msg resps
					  end

and binDispatch  fail succ (resp.resps) = case resp in 
					     Bn val       : succ val resps
					  || Failure msg  : fail msg resps
					  end

and succDispatch fail succ (resp.resps) = case resp in
					     Success     : succ resps
					  || Failure msg : fail msg resps
					  end

and strListDispatch fail succ (resp.resps) = case resp in
						StrList val : succ val resps
					     || Failure msg : fail msg resps
					     end

and sigActDispatch fail succ (resp.resps) = case resp in
						SigActResp val : succ val resps
					     || Failure msg : fail msg resps
					     end

and abort		:: FailCont
and abort msg	=  done

and exit		:: FailCont
and exit err	= let msg = case err in
			       ReadError s   : s
			    || WriteError s  : s
			    || SearchError s : s
			    || FormatError s : s
			    || OtherError s  : s
			    end
		  in appendChan stderr (msg @ "\n") abort done
/*
and print		:: (Text a) => a -> Dialog
and print x		=  appendChan stdout (show x) exit done
and prints          :: (Text a) => a -> String -> Dialog
and prints x s	=  appendChan stdout (shows x s) exit done
*/

and interact	:: (String -> String) -> Dialog
and interact f	=  readChan stdin exit
			    (\x . appendChan stdout (f x) exit done)

------------------------------------ hbc bonus
and type DblCont == Double -> Dialog

and sleep           :: Double ->           FailCont -> SuccCont    -> Dialog
and changeDirectory :: String ->           FailCont -> SuccCont    -> Dialog
and getTime         ::                     FailCont -> DblCont     -> Dialog
and deleteDirectory :: String ->           FailCont -> SuccCont    -> Dialog
and readDirectory   :: String ->           FailCont -> StrListCont -> Dialog
and getCpuTime      ::                     FailCont -> DblCont     -> Dialog
and getLocalTime    ::                     FailCont -> DblCont     -> Dialog
and readDirectory name fail succ resps =
    ReadDirectory name . strListDispatch fail succ resps

and getLocalTime fail succ resps =
    GetLocalTime . dblDispatch fail succ resps

and sleep dbl fail succ resps =
    Sleep dbl . succDispatch fail succ resps

and changeDirectory str fail succ resps =
    ChangeDirectory str . succDispatch fail succ resps

and deleteDirectory str fail succ resps =
    DeleteDirectory str . succDispatch fail succ resps

and getTime fail succ resps =
    GetTime . dblDispatch fail succ resps

and getCpuTime fail succ resps =
    GetCpuTime . dblDispatch fail succ resps

and dblDispatch  fail succ (resp.resps) = case resp in 
					     Dbl val      : succ val resps
					  || Failure msg  : fail msg resps
					  end
end
