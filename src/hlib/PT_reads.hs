module	P_Text_reads(reads) where
reads 	        :: (Text a) => ReadS a
reads s		=  readsPrec 0 s
