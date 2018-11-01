module PreludeX where
reads 	        :: (Read a) => ReadS a
reads s		=  readsPrec 0 s
