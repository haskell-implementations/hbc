/*
**	hiatonic:	change to hiatonic input mode.
**			hiatonic <delay>
**			hiatonic gives a timeout of <delay> milliseconds from when the reads starts.
**			mhiatonic delays a minimum of <delay>, but if the last read did not
**			time out whatever time is left over from that is used as delay.
*/

module
export hiatonic, mhiatonic;
    hiatonic d = [CHIATON; chr d]
and mhiatonic d = [CMHIATON; chr d]
end

