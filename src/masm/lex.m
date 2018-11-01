module -- lex
export lex;
rec
    Comment = ';'
and Newline = '\n'
and isword c = isalnum c | mem c "$._-+#@"
and getstring ('\\'.'\n'.r) = getstring r
||  getstring ('\\'.c.r) = '\\'.c.getstring r
||  getstring (   '"'.r) = ['"']
||  getstring (     c.r) = c.getstring r
and
    takeword [] = ([],[])
||  takeword (' '.l) = takeword l
||  takeword ('\n'.l) = takeword l
||  takeword ('\t'.l) = takeword l
||  takeword ('"'.l) = let s = getstring l in ('"'.s, tail (length s) l)
||  takeword (x.l) =
/*    if isdigit x then
        let (l1,l2) = take (\d.isdigit d) l
        in  (x.l1,l2)
    else*/ if isword x then
        let (l1,l2) = take isword l
        in  (x.l1,l2)
    else
	([x],l)
and nlconv ('&'.'&'.r) = Newline.nlconv r
||  nlconv (c.r) = c.nlconv r
||  nlconv [] = []
and lexline = filter (not o null) o choplist takeword o fst o splitat Comment
and lex = filter (not o null) o map lexline o choplist (splitat Newline) o nlconv
end
