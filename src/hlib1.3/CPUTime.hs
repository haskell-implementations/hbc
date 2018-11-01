module CPUTime where
import IO
import _LibIO__process
import _LibDialogue

getCPUTime :: IO Integer
getCPUTime = processRequestIODouble Nothing Nothing GetCpuTime >>= \ d -> return (truncate (d * fromInteger cpuTimePrecision))

cpuTimePrecision :: Integer
cpuTimePrecision = 1000000000000

getCPUTimeDouble :: IO Double
getCPUTimeDouble = processRequestIODouble Nothing Nothing GetCpuTime
