module FileStat(getFileStat, FileStat(..), FilePerm(..), FileMode(..)) where
import Time
import IO
import _LibIO__process
import _LibDialogue
import Word
import LMLuid

data FileMode = FFifo | FFchr | FFdir | FFblk | FFreg | FFlnk | FFsock | FSuid | FSgid | FSvtx deriving (Eq, Ord, Show)
data FilePerm = FPWrite | FPRead | FPExec deriving (Eq, Ord, Show)

data FileStat = FileStat {
	st_dev, st_ino :: Int,
	st_mode :: [FileMode],
	st_uperm, st_gperm, st_operm :: [FilePerm],
	st_nlink :: Int,
	st_uid, st_gid :: String,
	st_size :: Integer,
	st_atime, st_mtime, st_ctime :: ClockTime,
   } deriving (Eq, Ord, Show)

getFileStat :: FilePath -> IO FileStat
getFileStat name = 
	processRequestIOString Nothing (Just name) (StatusFile name) >>= \ s ->
	return (decodeStat (map read (words (drop 4 s))))

decodeStat :: [Int] -> FileStat
decodeStat [dev, ino, mode, nlink, uid, gid, rdev, size, atime, mtime, ctime] =
	let (m, (up,gp,op)) = decodeMode mode
	in  FileStat {
		st_dev = dev,
		st_ino = ino,
		st_mode = m,
		st_uperm = up,
		st_gperm = gp,
		st_operm = op,
		st_nlink = nlink,
		st_uid = strUid uid,
		st_gid = strGid gid,
		st_size = toInteger size,
		st_atime = convTime atime,
		st_mtime = convTime mtime,
		st_ctime = convTime ctime
	    }

convTime t = addToClockTime (TimeDiff{tdYear = 0, tdMon = 0, tdDay = 0, tdHour = 0, tdMin = 0, tdSec = t, tdPicosec = 0}) clockTime1970

clockTime1970 = toClockTime (CalendarTime{ctYear=1970, ctMon=January, ctDay=1, ctHour=0, ctMin=0, ctSec=0, ctPicosec=0, ctTZName="", ctTZ=0, ctIsDST=False})

strUid uid = uid2str uid
strGid gid = gid2str gid
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

