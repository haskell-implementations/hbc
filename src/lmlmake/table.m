module -- Table

#include "tree234.t"
#include "utils.t"

export table, mapTable, listTable, tableUpdate, tableLookup, emptyTable;

rec
type Table *a = T (Tree234 *a)!

and emptyTable = T initTree234

and tableLookup n j x (T t) = treeSearch n j (keyCmp (x,fail "tableLookup")) t

and tableUpdate x (T t) = T (update' x t)

and update' = treeAdd const keyCmp

and mapTable f (T t) = T (treeMap f t)

and listTable (T t) = treeList t

and table xs = T (treeFromList const keyCmp xs)

and keyCmp (a, _) (b, _) lt eq gt =
    if a = b then eq else if a < b then lt else gt

end
