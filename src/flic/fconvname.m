module
export convname;
rec
   convname (n as ('_'.m))  =  assocdef m Uconv n
|| convname (n as ('P'.m))  =  assocdef m PFconv n
|| convname (n as ('F'.m))  =  assocdef m PFconv n
|| convname n               =  n
and
   Uconv =
	["false",      "FALSE";
	 "true",       "TRUE";
	 "ord",        "TAG-ENUM";
	 "chr",        "ENUM";
	 "fail",       "FAIL";         -- defined in flic prelude
	 "eagerapp",   "[!]";          -- made into annotation in fconv
	 "eagerlam",   "[LAM!]";       -- ditto
	 "uneager",    "[*]"]          -- ditto
and
   PFconv =
	["cons",       "PACK-2-1";
	 "comma",      "PACK-2-1";
	 "neg",        "INT_";
	 "add",        "INT+";
	 "sub",        "INT-";
	 "mul",        "INT*";
	 "div",        "INT/";
	 "mod",        "INT%";
	 "lt",         "INT<";
	 "le",         "INT<=";
	 "eq",         "INT=";
	 "ge",         "INT>=";
	 "gt",         "INT>";
	 "ne",	       "INT!=";
	 "bigne",      "POLY!=";
	 "biglt",      "POLY<";
	 "bigle",      "POLY<=";
	 "bigeq",      "POLY=";
	 "bigge",      "POLY>=";
	 "biggt",      "POLY>";
	 "bigne",      "POLY!=";
	 "not",        "NOT";
	 "or",         "OR";
	 "and",        "AND";
	 "seq",        "SEQ";
	 "comp",       "COMP";         -- defined in flic prelude
	 "fail",       "FAIL";         -- ditto
	 "conc",       "CONC";         -- ditto
	 "s2_1",       "SEL-2-0";
	 "s2_2",       "SEL-2-1";
	 "s3_1",       "SEL-3-0";
	 "s3_2",       "SEL-3-1";
	 "s3_3",       "SEL-3-2";
	 "s4_1",       "SEL-4-0";
	 "s4_2",       "SEL-4-1";
	 "s4_3",       "SEL-4-2";
	 "s4_4",       "SEL-4-3";
	 "s5_1",       "SEL-5-0";
	 "s5_2",       "SEL-5-1";
	 "s5_3",       "SEL-5-2";
	 "s5_4",       "SEL-5-3";
	 "s5_5",       "SEL-5-4";
	 "s6_1",       "SEL-6-0";
	 "s6_2",       "SEL-6-1";
	 "s6_3",       "SEL-6-2";
	 "s6_4",       "SEL-6-3";
	 "s6_5",       "SEL-6-4";
	 "s6_6",       "SEL-6-5";
	 "#2",         "PACK-2-0";
	 "#3",         "PACK-3-0";
	 "#4",         "PACK-4-0";
	 "#5",         "PACK-5-0";
	 "#6",         "PACK-6-0";
	 "#7",         "PACK-7-0";
	 "#8",         "PACK-8-0";
	 "#9",         "PACK-9-0";
	 "#10",        "PACK-10-0";
	 "#11",        "PACK-11-0";
	 "#12",        "PACK-12-0";
	 "#13",        "PACK-13-0";
	 "#14",        "PACK-14-0";
	 "#15",        "PACK-15-0"]
end
