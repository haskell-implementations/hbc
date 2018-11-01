#define floorDouble2Int {-:"PfloorDouble2Int":-}
#define ceilingDouble2Int {-:"PceilingDouble2Int":-}
#define floorFloat2Int {-:"PfloorFloat2Int":-}
#define ceilingFloat2Int {-:"PceilingFloat2Int":-}
module RF_Spec(floorDouble2Int, ceilingDouble2Int, floorFloat2Int, ceilingFloat2Int) where

floorDouble2Int, ceilingDouble2Int{-, roundDouble2Int-} :: Double -> Int
floorFloat2Int, ceilingFloat2Int{-, roundFloat2Int-} :: Float -> Int

floorDouble2Int x =
	let i = truncate x
	in  if fromInt i == x then i
	    else if i < 0 then i-1 else i

ceilingDouble2Int x =
	let i = truncate x
	in  if fromInt i == x then i
	    else if i < 0 then i else i+1

floorFloat2Int x =
	let i = truncate x
	in  if fromInt i == x then i
	    else if i < 0 then i-1 else i

ceilingFloat2Int x =
	let i = truncate x
	in  if fromInt i == x then i
	    else if i < 0 then i else i+1
