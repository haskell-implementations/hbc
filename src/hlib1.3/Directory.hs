module Directory(
	Permissions(..), getPermissions, setPermissions,
	createDirectory, removeDirectory, removeFile,
	renameDirectory, renameFile,
	getDirectoryContents, getCurrentDirectory, setCurrentDirectory,
	doesFileExist, doesDirectoryExist,
	getModificationTime
	)where
import IO
import _LibIO__process
import _LibDialogue
import Time(ClockTime, _mkClockTime)

data Permissions = Permissions {
        readable, writable, executable, searchable :: Bool
    } deriving (Eq, Ord, Show, Read)

createDirectory :: FilePath -> IO ()
createDirectory name = processRequestIOUnit Nothing (Just name) (CreateDirectory name "")

removeDirectory :: FilePath -> IO ()
removeDirectory name = processRequestIOUnit Nothing (Just name) (DeleteDirectory name)

removeFile :: FilePath -> IO ()
removeFile name = processRequestIOUnit Nothing (Just name) (DeleteFile name)

renameDirectory :: FilePath -> FilePath -> IO ()
renameDirectory old new = processRequestIOUnit Nothing (Just old) (RenameFile old new)

renameFile :: FilePath -> FilePath -> IO ()
renameFile old new = processRequestIOUnit Nothing (Just old) (RenameFile old new)

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents name = processRequestIOStringList Nothing (Just name) (ReadDirectory name)

getCurrentDirectory :: IO FilePath
getCurrentDirectory = processRequestIOString Nothing Nothing GetCurrentDirectory

setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory name = processRequestIOUnit Nothing (Just name) (ChangeDirectory name)

doesFileExist :: FilePath -> IO Bool
doesFileExist name = does name 0x8

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist name = does name 0x4

does :: FilePath -> Int -> IO Bool
does name m =
	getFileStat name >>= \ [dev, ino, mode, nlink, uid, gid, rdev, size, atime, mtime, ctime] ->
	return ((mode `div` 0x1000) `mod` 0x10 == m)
    `catch`
        \ ioerr ->
	if isDoesNotExistError ioerr then
	    return False
	else
	    fail ioerr
	
getPermissions :: FilePath -> IO Permissions
getPermissions name = 
	processRequestIOInt Nothing (Just name) (H_GetPermissions name) >>= \ perm ->
	let has n p = (p `div` n) `mod` 2 == 1 in
	return (Permissions { readable = has 4 perm,
			      writable = has 2 perm,
			      executable = has 1 perm && not (has 8 perm),
			      searchable = has 1 perm && has 8 perm })

setPermissions :: FilePath -> Permissions -> IO ()
setPermissions name perm = 
	getFileStat name >>= \ [dev, ino, mode, nlink, uid, gid, rdev, size, atime, mtime, ctime] ->
	let p = (if readable perm then 4 else 0) + (if readable perm then 2 else 0) + (if readable perm || searchable perm then 1 else 0)
	    mode' = (mode `div` 0o1000) * 0o1000 + p * 0o100 + mode `mod` 0o100
	in  processRequestIOUnit Nothing (Just name) (H_Chmod name mode')

getModificationTime :: FilePath -> IO ClockTime
getModificationTime name =
	getFileStat name >>= \ [dev, ino, mode, nlink, uid, gid, rdev, size, atime, mtime, ctime] ->
	return (_mkClockTime (fromInt mtime))

getFileStat :: FilePath -> IO [Int]
getFileStat name = 
	processRequestIOString Nothing (Just name) (StatusFile name) >>= \ s ->
	return (map read (words (drop 4 s)))

