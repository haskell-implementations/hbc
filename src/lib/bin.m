/*
**	bin:		Binary I/) stuff.
**
*/
module
export showBin, nullBin, readBin, isNullBin;
rec showBin x xs = x.xs
and nullBin = []
and readBin (x.xs) = (x, xs)
and isNullBin [] = true
||  isNullBin _ = false
end
