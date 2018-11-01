module PreludeX((,)) where
import Ix

instance (Ix a, Ix b) => Ix (a, b) where
    range   ((lx,ly), (ux,uy)) = [ (x,y) | x<-range(lx, ux), y<-range(ly, uy) ]
    index   ((lx,ly), (ux,uy)) (x, y) = index (lx,ux) x * (index (ly,uy) uy + 1) + index (ly, uy) y
    inRange ((lx,ly), (ux,uy)) (x, y) = inRange (lx,ux) x && inRange (ly,uy) y


instance Ix (Int, Int) where
    range   ((lx,ly), (ux,uy)) = [ (x,y) | x<-[lx..ux], y<-[ly..uy] ]
    index   ((lx,ly), (ux,uy)) (x, y) = (x-lx) * (uy-ly+1)  +  (y - ly)
    inRange ((lx,ly), (ux,uy)) (x, y) = lx <= x && x <= ux  &&  ly <= y && y <= uy

