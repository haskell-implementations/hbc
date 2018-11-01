module
#include "../lib/dialog.t"
nonfix "show_#2";
nonfix "show_#3";
nonfix "show_#4";
nonfix "show_#5";
nonfix "show_#6";
nonfix "show_#7";
nonfix "show_#8";
#ifdef CURRY
#define SEP ", "
#else
#define SEP "; "
#endif
export show_List, show_Int, show_Bool, show_Char, show_#2, show_#3, show_#4,
	show_#5, show_#6, show_#7, show_#8, show_LArray, show_OK, show_Double,
	show_Integer, show_String,
	printString;
rec show_List f l = '['.mix (map f l) SEP @ "]"
and show_Int = show_int
and show_LArray f a = 
#if 1
	"{("@show_Int (lowerbound a)@","@
	     show_Int (upperbound a)@"); "@
        mix [f (a?i);;i<-[lowerbound a .. upperbound a]] "," @ "}"
#else
"Array"
#endif
#ifdef CURRY
and show_Bool true = "True"
||  show_Bool false = "False"
and chars = ["NUL";"SOH";"STX";"ETX";"EOT";"ENQ";"ACK";"a";
	     "b";  "t";  "n";  "VT"; "f";  "r";  "SO"; "SI";
	     "DLE";"DC1";"DC2";"DC3";"DC4";"NAK";"SYN";"ETB";
	     "CAN";"EM"; "SUB";"e";  "FS"; "GS" ;"RS"; "US"]
and hex i = "?!"
and show_Char c = 
	let i = ord c in
	if i >= 0 & i < 32 then
	    "'\\"@select (i+1) chars @ "'"
	else if c = '\'' then
	    "'\\''"
	else if i < 128 then
	    ['\'';c;'\'']
	else
	    "'\\x"@hex i@"'"
#else
and show_Bool = show_bool
and show_Char = show_char
#endif
and show_#2 f g (x,y) = "("@f x@","@g y@")"
and show_#3 f1 f2 f3 (x1,x2,x3) =
    "("@f1 x1@","@f2 x2@","@f3 x3@")"
and show_#4 f1 f2 f3 f4 (x1,x2,x3,x4) =
    "("@f1 x1@","@f2 x2@","@f3 x3@","@f4 x4@")"
and show_#5 f1 f2 f3 f4 f5 (x1,x2,x3,x4,x5) =
    "("@f1 x1@","@f2 x2@","@f3 x3@","@f4 x4@","@f5 x5@")"
and show_#6 f1 f2 f3 f4 f5 f6 (x1,x2,x3,x4,x5,x6) =
    "("@f1 x1@","@f2 x2@","@f3 x3@","@f4 x4@","@f5 x5@","@f6 x6@")"
and show_#7 f1 f2 f3 f4 f5 f6 f7 (x1,x2,x3,x4,x5,x6,x7) =
    "("@f1 x1@","@f2 x2@","@f3 x3@","@f4 x4@","@f5 x5@","@f6 x6@","@f7 x7@")"
and show_#8 f1 f2 f3 f4 f5 f6 f7 f8 (x1,x2,x3,x4,x5,x6,x7,x8) =
    "("@f1 x1@","@f2 x2@","@f3 x3@","@f4 x4@","@f5 x5@","@f6 x6@","@f7 x7@","@f8 x8@")"
and show_OK f g (No a) = "(No "@f a@")"
||  show_OK f g (Yes a) = "(Yes "@g a@")"
and show_Double f = ftos f
and show_Integer i = Iitos i@"#"
and show_Option f None = "None"
||  show_Option f (Some x) = "(Some "@f x@")"
and show_String s = show_string s

and printString s = \ _ . [AppendChan "stdout" s]
end
