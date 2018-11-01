module Main(main) where {
tests = [
--nullBin
--isNullBin
("True",        True),
("not 1",       not False),
("not 2",       not True == False),
("ord",		ord 'A' == 65),
("chr",		chr 97 == 'a'),
("all 1",       all (\x->x/='a') "hello"),
("all 2",       not (all (\x->x/='a') "hallo")),
("and 1",	and [True,True]),
("and 2",       not (and [True, False])),
("any 1",       any (\x->x=='l') "hello"),
("any 2",       not (any (\x->x=='x') "hello")),
("break 1",     break (\x->x=='c') "abcde" == ("ab", "cde")),
("break 2",     break (\x->x=='c') "" == ("", "")),
("++",          "foo"++"bar" == "foobar"),
("concat",      concat ["x","yy","zzz"] == "xyyzzz")
--("cycle",       take 10 (cycle "abc") == "abcabcabca"),
("\\",          True),

("",            True)
];

runtests [] = "All Prelude tests ok\n";
runtests ((s, False):_) = "Prelude test failed: " ++ s ++ "\n";
runtests (_:ts) = runtests ts;

main _ = [AppendChan stdout (runtests tests)]
}

