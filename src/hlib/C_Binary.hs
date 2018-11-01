module C_Binary(Binary(..)) where
class  Binary a  where
    readBin		:: Bin -> (a,Bin)
    showBin		:: a -> Bin -> Bin
