/*
**	nameSupply:		generate unique names
**
**		initialNameSupply 	is an infinite set (of Int)
**					[like argv etc, this is not a real constant but a
**					 value supplied when the program is run]
**		getName s		returns an element of s
**		splitNames s		returns two infinite disjoint subsets of s
**					neither of which contain getName s
**		This is an efficient implementation of the routines in
**		chap 9 (by P Hancock) in Peyton Jones
**		"The implementation of functional programming languages"
*/
module
import __gensym:*a->Int {# ARITY _ = 1 #};		-- gensym is written in M-code, see gensym.M
export initialNameSupply, getName, splitNames;
rec type nameSupply = Node Int nameSupply nameSupply
and initialNameSupply = gen 0
and gen n = Node (__gensym n) (gen n) (gen n)  -- dubious
and splitNames (Node _ s1 s2) = (s1, s2)
and getName (Node k _ _) = k
end
