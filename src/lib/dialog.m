/*
**	dialog:		Haskell I/O.
**
*/
module
export Request, Response, IOError, Dialog, stdin, stdout, stderr, stdecho, SigAct, Exception,excError,excInterrupt,excTerminate,excHangup,excPipe,excArithmetic;
rec type Request =
    ReadFile 	    String +
    WriteFile	    String String +
    AppendFile	    String String +
    ReadBinFile	    String +
    WriteBinFile    String Bin +
    AppendBinFile   String Bin +
    DeleteFile	    String +
    StatusFile      String +
    ReadChan        String +
    AppendChan      String String +
    ReadBinChan     String +
    AppendBinChan   String Bin +
    StatusChan      String +
    Echo	    Bool +
    GetArgs         +
    GetEnv	    String +
    SetEnv          String String +
    ReadChannels    (List String) +
    ReadBinChannels (List String) +
    CreateProcess   Dialog +
    CreateDirectory String String +
    /* junk! */
    OpenFile	    String Bool +
    OpenBinFile     String Bool +
    CloseFile       File +
    ReadVal         File +
    ReadBinVal      File +
    WriteVal        File Char +
    WriteBinVal     File Bin +
    /* extra */
    Sleep	    Double +
    ChangeDirectory String +
    GetTime	    +
    DeleteDirectory String +
    System          String +
    ReadDirectory   String +
    XCommand        (DisplayT#Window#XCommand) +  -- CHANGED (TH 940323)
    GetAsyncInput   +                           -- CHANGED (TH 940323)
    GetCpuTime      +
    GetProgName     +
    GetLocalTime    +
    SigAction Exception SigAct +
    Exit Int	    +
    ReadFileScattered String (List Int) +
    Select          (List Descriptor) +       -- NEW (TH 940323)
    SocketRequest   SocketRequest +           -- NEW (TH 940323)
    XRequest        (DisplayT#Window#XRequest) + -- NEW (TH 940323)
    ReadFileFast    String +

    RenameFile String String +
    GetCurrentDirectory 

  -- Haskell 1.3 I/O
  + H_OpenFile String Int		-- return Fil
  + H_Close File			-- return Success
  + H_FileSize File		-- return IntResp
  + H_IsEOF File			-- return IntResp
  + H_SetBuffering File Int	-- return Success
  + H_GetBuffering File		-- return IntResp
  + H_Flush File			-- return Success
  + H_Seek File Int Int		-- return IntResp
  + H_GetFlags File		-- return IntResp
  + H_GetChar File		-- return IntResp
  + H_UnGetChar File Int		-- return Success
  + H_PutChar File Int		-- return Success
  + H_PutString File String	-- return Success
  + H_GetFile File		-- return Str
  + H_Select ([File] # [File] # [Double]) -- return SelectResp, last list is a Maybe
  + H_CCall _CPointer [_CUnion] _CUnion -- return CCallResp
  + __RunAnswer __Answer		-- never returns 

and type __Answer = __Answer *a

and type _CUnion  =		  _CUInt Int
			+ _CUDouble Double 
			+ _CUString String 
			+ _CUPointer _CPointer

and type _CPointer = _CPointer Int!

and type SigAct = SAIgnore + SADefault + SACatch (String -> Dialog)

and type Exception == Int
and excError :: Exception
and excInterrupt :: Exception
and excTerminate :: Exception
and excHangup :: Exception
and excPipe :: Exception
and excArithmetic :: Exception
and excError     = 0
and excHangup    = 1
and excInterrupt = 2
and excArithmetic= 8
and excPipe      = 13
and excTerminate = 15

and type Response =
    Success         +
    Str             String +
    Bn Bin          +
    Failure         IOError +
    Tag             (List (String#Char)) +
    BinTag          (List (String#Bin)) +
    StrList	    (List String) +
    Fil             File +
    Dbl		    Double +
    AsyncInput      AsyncInput +               -- CHANGED (TH 940323,mc 940331)
    SocketResponse  SocketResponse +           -- NEW (TH 940323)
    XResponse       XResponse +                -- NEW (TH 940323)
    IntResp Int +
    SelectResp [([File] # [File] # [Double])] +
    SigActResp SigAct +
    CCallResp _CUnion

and type Dialog == (List Response) -> (List Request)
and type IOError =
    WriteError	    String +
    ReadError       String +
    SearchError     String +
    FormatError     String +
    OtherError      String

and stdin =         "stdin"
and stdout =        "stdout"
and stderr =        "stderr"
and stdecho =       "stdecho"

end

