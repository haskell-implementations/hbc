/*
**	permutations:
**		Gives a list of all permutations of a given list
**
**		permutations [1;2;3] =
**		  [[1;2;3];[2;1;3];[2;3;1];[1;3;2];[3;1;2];[3;2;1]]
*/
module
-- WARNING: not self contained
export permutations;
rec
  insert x [] = [[x]]
  ||
  insert x (h.t) =
    (x.h.t). map (\m.h.m) (insert x t)
and
  permutations [] = [[]]
  ||
  permutations (h.t) = concmap (insert h) (permutations t)
end
