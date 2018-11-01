module IO(openFile, getFile, getEnvi, toStdout, toStderr, toFile, toFileAppend,
	  FileMode(..), FilePerm(..), FileStat(..), decodeStat, statFile, openDirectory, 
	  setCbreak, setCooked, setUnbuffered, doFlush, progArgs, progName) where
import LMLopenfile
import LMLstatfile
import LMLgetenv
import LMLfile
import LMLopendir
import LMLargv
import LMLprogname
import LMLuid
import Either
import Maybe
import Word
import ListUtil

data FileMode = FFifo | FFchr | FFdir | FFblk | FFreg | FFlnk | FFsock | FSuid | FSgid | FSvtx deriving (Eq, Ord, Text)
data FilePerm = FPWrite | FPRead | FPExec deriving (Eq, Ord, Text)
data FileStat = FileStat Int Int [FileMode] ([FilePerm],[FilePerm],[FilePerm]) Int String String Integer Double Double Double 
		deriving (Eq, Ord, Text)

openFile :: String -> Either String String
openFile f = openfile f

openDirectory :: String -> Either String [String]
openDirectory f = opendir f

statFile :: String -> Either String [Int]
statFile f = statfile f

decodeStat :: [Int] -> FileStat
decodeStat [dev, ino, mode, nlink, uid, gid, rdev, size, atime, mtime, ctime] =
	let (m, p) = decodeMode mode
	in  FileStat dev ino m p nlink (strUid uid) (strGid gid) (toInteger size) (fromInt atime) (fromInt mtime) (fromInt ctime)

strUid uid = {-show uid-} uid2str uid
strGid gid = {-show gid-} gid2str gid
hasbits []              w = []
hasbits ((b, m, v):bvs) w = if w `bitAnd` m == b then v:hasbits bvs w else hasbits bvs w
perm w = hasbits [(4, 4, FPRead), (2, 2, FPWrite), (1, 1, FPExec)] w
decodeMode m = 
	let w = fromInt m :: Word
	    o = w
	    g = w `bitRsh` 3
	    u = w `bitRsh` 6
	in  (hasbits [(0x1000, 0xf000, FFifo), (0x2000, 0xf000, FFchr), (0x4000, 0xf000, FFdir), (0x6000, 0xf000, FFblk), (0x8000, 0xf000, FFreg), 
	              (0xa000, 0xf000, FFlnk), (0xc000, 0xf000, FFsock), (0x0800, 0x0800, FSuid), (0x0400, 0x0400, FSgid), (0x0200, 0x0200, FSvtx)] w,
	     (perm u, perm g, perm o))

getFile :: String -> String
getFile f =
	case openFile f of
	Left  msg -> error msg
	Right con -> con

getEnvi :: String -> Maybe String
getEnvi s = assocDef env Nothing s
	where env = map (\s->let (a,b) = span (/= '=') s in (a, Just (tail b))) envp

toStdout, toStderr :: String
toStdout = [TOSTDOUT]
toStderr = [TOSTDERR]

toFile, toFileAppend :: String -> String
toFile s = TOFILE : s ++ "\n"
toFileAppend s = TOFILEA : s ++ "\n"

setCbreak :: String
setCbreak = [CCBREAK]

setCooked :: String
setCooked = [CCOOKED]

setUnbuffered :: String
setUnbuffered = [CUNBUFF]

doFlush :: String
doFlush = [CFLUSH]

progArgs :: [String]
progArgs = argv

progName :: String
progName = progname
