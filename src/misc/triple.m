module
export fst3, snd3, thd3, combine3, mapfst3;
rec fst3 (x,_,_) = x
and snd3 (_,x,_) = x
and thd3 (_,_,x) = x
and combine3 (x.xs, y.ys, z.zs) = (x,y,z) . combine3 (xs,ys,zs)
||  combine3 _ = []
and mapfst3 f l = [(f x, y, z) ;; (x, y, z) <- l]
end
