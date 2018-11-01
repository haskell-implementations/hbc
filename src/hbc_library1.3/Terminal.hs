module Terminal(setRaw, setCooked, readLine) where
import CCall
import Char
import StdIO

setRaw, setCooked :: IO ()
setRaw = setRC True
setCooked = setRC False
setRC :: Bool -> IO ()
setRC b = ccall set_tty b

-- TODO: TAB-expansion.
-- Read a line from stdin with fancy editing.
-- Args: Prompt, initial line contents, history.
readLine :: String -> String -> [String] -> IO String
readLine prompt start hist = 
    do
	setRaw
	hSetBuffering stdout NoBuffering
	putStr prompt
	s <- readlpr start (reverse hist, "", [])
	setCooked
	return s

readlpr :: String -> ([String], String, [String]) -> IO String
readlpr start hist =
    do
        putFmtStr start
	readl start (length start) (length start) hist

putFmtStr = putStr . fmtStr
putFmtChar = putStr . fmtChar
lenOf = length . fmtStr
lenOfC = length . fmtChar
fmtStr = concatMap fmtChar
fmtChar c = 
    if isPrint c then [c] else ['^', toEnum (fromEnum c + fromEnum '@')]

readl :: String -> Int -> Int -> ([String], String, [String]) -> IO String
readl cur len pos hist =
  let startOfLine = backwards (lenOf (take pos cur))
      eraseLine = do startOfLine; eraseChars (lenOf cur)
      endOfLine =
	    let f n = if n == len then return ()
	    	      else do putStr (fmtChar (cur!!n)); f (n+1)
	    in  f pos
      eraseChar p =
	    do
	        let tl = drop (p+1) cur
	        let cur' = take p cur ++ tl
		putFmtStr tl
		putStr (replicate (lenOfC (cur!!p)) ' ')
		backwards (lenOf (drop p cur))
		readl cur' (len-1) p hist
      insChar c =
	    do
		let cur' = insert pos c cur
		putFmtStr (drop pos cur')
		backwards (lenOf (drop (pos+1) cur'))
		readl cur' (len+1) (pos+1) hist

      backthis = backwards (lenOfC (cur!!(pos-1)))
  in
    do
      c <- getChar
      c' <- arrowCheck c
      case c' of
	'\n' -> doneit cur
	'\r' -> doneit cur
	'\b' | pos > 0 -> 
	    do
	        backthis
		eraseChar (pos-1)
	'\^D' | pos < len -> eraseChar pos
	'\^F' | pos < len -> 
	    do
	        putFmtChar (cur!!pos)
		readl cur len (pos+1) hist
	'\^B' | pos > 0 ->
	    do
	        backthis
		readl cur len (pos-1) hist
	'\^A' ->
	    do
		startOfLine
		readl cur len 0 hist
	'\^E' ->
	    do
		endOfLine
		readl cur len len hist
	'\^K' ->
	    do
		eraseChars (lenOf (drop pos cur))
		readl (take pos cur) pos pos hist
	'\^P' ->
	    case hist of
	    ([], _, _) -> readl cur len pos hist
	    (p:ps, n, ns) -> do eraseLine; readlpr p (ps, p, n:ns)
	'\^N' ->
	    case hist of
	    (_, _, []) -> readl cur len pos hist
	    (ps, p, n:ns) -> do eraseLine; readlpr n (p:ps, n, ns)
	'\^Q' ->
	    do c <- getChar; insChar c
	c | isPrint c -> insChar c
	_ -> readl cur len pos hist

-- Translate arrow keys to something simple.
arrowCheck :: Char -> IO Char
arrowCheck c@'\ESC' =
    do
	d <- getChar
	if d /= '[' then return c else
	    do
		e <- getChar
		case e of
		    'C' -> return '\^F'
		    'D' -> return '\^B'
		    'A' -> return '\^P'
		    'B' -> return '\^N'
		    _ -> return c
arrowCheck c = return c

doneit str =
    do
	putStr "\n"
	return str

backwards :: Int -> IO ()
backwards n = sequence (replicate n (putChar '\b'))

eraseChars n =
    do
	sequence (replicate n (putChar ' '))
	backwards n


insert :: Int -> a -> [a] -> [a]
insert 0 x xs = x:xs
insert n x (y:ys) = y : insert (n-1) x ys
