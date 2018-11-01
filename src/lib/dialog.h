#include "../runtime/node.h"

/* type Request */
#define ReadFile 0
#define WriteFile 1
#define AppendFile 2
#define ReadBinFile 3
#define WriteBinFile 4
#define AppendBinFile 5
#define DeleteFile 6
#define StatusFile 7
#define ReadChan 8
#define AppendChan 9
#define ReadBinChan 10
#define AppendBinChan 11
#define StatusChan 12
#define Echo 13
#define GetArgs 14
#define GetEnv 15
#define SetEnv 16
#define ReadChannels 17
#define ReadBinChannels 18
#define CreateProcess 19
#define CreateDirectory 20
#define OpenFile 21
#define OpenBinFile 22
#define CloseFile 23
#define ReadVal 24
#define ReadBinVal 25
#define WriteVal 26
#define WriteBinVal 27
/* extra */
#define Sleep 28
#define ChangeDirectory 29
#define GetTime 30
#define DeleteDirectory 31
#define System 32
#define ReadDirectory 33
#define XCommand 34
#define GetAsyncInput 35
#define GetCpuTime 36
#define GetProgName 37
#define GetLocalTime 38
#define SigAction 39
#define Exit 40
#define ReadFileScattered 41
#define Select 42
#define SocketRequest 43
#define XRequest 44
#define ReadFileFast 45
#define RenameFile 46
#define GetCurrentDirectory 47

#define H_OpenFile 48
#define H_Close 49
#define H_FileSize 50
#define H_IsEOF 51
#define H_SetBuffering 52
#define H_GetBuffering 53
#define H_Flush 54
#define H_Seek 55
#define H_GetFlags 56
#define H_GetChar 57
#define H_UnGetChar 58
#define H_PutChar 59
#define H_PutString 60
#define H_GetFile 61
#define H_Select 62
#define H_CCall 63
#define RunStop 64
#define H_GetTimeZone 65
#define H_GetErrno 66
#define H_GetPermissions 67
#define H_Chmod 68

#define XMKRESP 0x80000000

/* type Response */
#define RSuccess 0
#define Str 1
#define Bn 2
#define Failure 3
#define Tagg 4
#define BinTag 5
#define StrList 6
#define Fil 7
#define Dbl 8
#define AsyncInput 9
#define SocketResponse 10
#define XResponse 11
#define IntResp 12
#define SelectResp 13
#define SigAc 14
#define CCallResp 15
#define GetTimeResp 16

/* type IOError */
#define WriteError 0
#define ReadError 1
#define SearchError 2
#define FormatError 3
#define OtherError 4

#define SAIgnore 0
#define SADefault 1
#define SACatch 2

extern void mkerrresp();

extern PTR *ep;

#define SOCKNAME "//SOCKET="
#define SOCKNAMELEN 9
