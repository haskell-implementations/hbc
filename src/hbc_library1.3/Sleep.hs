module Sleep(sleep) where
import SelectIO

sleep :: Double -> IO ()
sleep d = select ([], [], Just d) >> return ()
