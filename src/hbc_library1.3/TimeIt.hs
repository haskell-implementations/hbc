module TimeIt where
import CPUTime
import Time
import Printf

timeIO :: IO a -> IO a
timeIO io =
    do
	wall1 <- getClockTime
	cpu1 <- getCPUTimeDouble
	x <- io
	cpu2 <- getCPUTimeDouble
	wall2 <- getClockTime
	let wallDiff = timeDiffToDouble (diffClockTime wall2 wall1)
	    cpuDiff = cpu2 - cpu1
	putStr (printf "Elapsed time: %4.2fs CPU, %4.2fs real\n" [UDouble cpuDiff, UDouble wallDiff])
        return x

time :: (Show a) => a -> IO ()
time = timeIO . print
