module	PreludeX(Show(..)) where
class  Show a  where
    showsPrec :: Int -> a -> ShowS
    showList  :: [a] -> ShowS
    showsType :: a -> ShowS

    showsPrec _ x = showString "<<" . showsType x . showString ">>"

    showList []	= showString "[]"
    showList (x:xs)
		= showChar '[' . shows x . showl xs
		  where showl []     = showChar ']'
			showl (x:xs) = showString ", " . shows x . showl xs

shows x s	=  showsPrec 0 x s

