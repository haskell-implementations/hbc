module
export Pfail_funny_bounds,
       Pfail_index_too_small,
       Pfail_index_too_big;

	Pfail_funny_bounds _lo _hi =
		Pfail ("assocarray: funny bounds")
and	Pfail_index_too_small _siz _a _i = 
		Pfail (	"arrayindexing: index too small")
and	Pfail_index_too_big _siz _a _i = 
		Pfail (	"arrayindexing: index too big")
end
