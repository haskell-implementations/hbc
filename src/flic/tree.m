let type rec
   tree *a  =  Leaf *a + Branch *a (tree *a) (tree *a)
in
let rec
   treesum (Leaf x)  =  x
|| treesum (Branch x xt yt)  =  x + treesum xt + treesum yt
and
   treeforce (Leaf x)  =  Leaf x
|| treeforce (Branch x xt yt) = Branch x {treeforce xt} {treeforce yt}
and
   treegen 0  =  Leaf 3
|| treegen 1  =  Leaf 5
|| treegen d  =  Branch (8*d) (treegen (d-1)) (treegen (d-2))
in
  treesum (treeforce (treegen 10))
