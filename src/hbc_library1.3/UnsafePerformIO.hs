module UnsafePerformIO where
import _LibIO_all(_unsafePerformIO, IO)

unsafePerformIO :: IO a -> a
unsafePerformIO io = _unsafePerformIO io

