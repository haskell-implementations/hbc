module Numeric__showSigned where

showInt    :: Integral a => a -> ShowS
showInt n r = if n < 0 then "Numeric.showInmt < 0" else showInt' n r
showInt'   :: Integral a => a -> ShowS
showInt' n r = 
	let (n',d) = quotRem n 10
            r'     = toEnum (fromEnum '0' + fromIntegral d) : r
        in  if n' == 0 then r' else showInt' n' r'
