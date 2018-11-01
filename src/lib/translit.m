/*
**	translit:		do substitutions on a list. Given a list of
**				pairs of items and a list of items, all items
**				in second list that are fst components in a pair
**				in first list are substituted for snd component
**				in that pair.
**
**		translit ['a','A'; 'c','C'] "abcd" gives "AbCd"
*/
module
-- WARNING: not self contained
export	translit;
translit pairlist itemlist = map (\x.assocdef x pairlist x) itemlist
end
