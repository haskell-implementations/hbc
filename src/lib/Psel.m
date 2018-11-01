/*
**	Psel:		lml part of the selector functions.
*/
module
export
    Ps2_1, Ps2_2,
    Ps3_1, Ps3_2, Ps3_3,
    Ps4_1, Ps4_2, Ps4_3, Ps4_4,
    Ps5_1, Ps5_2, Ps5_3, Ps5_4, Ps5_5,
    Ps6_1, Ps6_2, Ps6_3, Ps6_4, Ps6_5, Ps6_6;
    Ps2_1 (x,_) = x
and Ps2_2 (_,x) = x
and Ps3_1 (x,_,_) = x
and Ps3_2 (_,x,_) = x
and Ps3_3 (_,_,x) = x
and Ps4_1 (x,_,_,_) = x
and Ps4_2 (_,x,_,_) = x
and Ps4_3 (_,_,x,_) = x
and Ps4_4 (_,_,_,x) = x
and Ps5_1 (x,_,_,_,_) = x
and Ps5_2 (_,x,_,_,_) = x
and Ps5_3 (_,_,x,_,_) = x
and Ps5_4 (_,_,_,x,_) = x
and Ps5_5 (_,_,_,_,x) = x
and Ps6_1 (x,_,_,_,_,_) = x
and Ps6_2 (_,x,_,_,_,_) = x
and Ps6_3 (_,_,x,_,_,_) = x
and Ps6_4 (_,_,_,x,_,_) = x
and Ps6_5 (_,_,_,_,x,_) = x
and Ps6_6 (_,_,_,_,_,x) = x
end
