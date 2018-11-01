module NonStdTrace(trace) where
import LMLTrace
--@@ Side effect tracing!
trace :: String -> a -> a
trace s x = LMLTrace.trace s x
