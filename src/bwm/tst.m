module
export f;
rec f n m z =
    let x = K1 m n
    and y = K2 m n
    and w = tl z in
    case cnc w w in
	[] : (x,y,[])
    ||  _.xs  : (n,y,xs)
    end
and tl (_.xs) = xs
||  tl _ = []
and cnc [] ys = ys
||  cnc (x.xs) ys = x.cnc xs ys
and K1 x y = x
and K2 x y = y
end
