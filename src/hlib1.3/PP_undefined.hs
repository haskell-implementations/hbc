module PreludeX where
undefined :: a
undefined = error "Prelude.undefined"

_undefined :: String -> Int -> a
_undefined file line = error ("Prelude.undefined used in file " ++ show file ++ " at line " ++ show line)
