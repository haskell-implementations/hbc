--
--@@ Graphs and DFS-based algorithms on them based
--@@ on Launchbury and King.
--@@ <p>
--@@ Uses monadic mutable arrays.
-- 
module Graph(
	Graph(..), Edge(..),
	buildG,
	vertices, edges, outdegree, indegree, transposeG, reverseE,
	preOrd, postOrd,
	topSort, scc,
	tabulate,
	reachable, path
       ) where
import Array
import State
import MutArray

type Table v a = Array v a
type Graph v = Table v [v]

vertices :: (Ix v) => Graph v -> [v]
vertices g = indices g

type Edge v = (v, v)

edges :: (Ix v) => Graph v -> [Edge v]
edges g = [ (v, w) | v <- vertices g, w <- g!v ]

mapT :: (Ix v) => (v -> a -> b) -> Table v a -> Table v b
mapT f t = array (bounds t) [(v, f v (t!v)) | v <- indices t]

type Bounds v = (v, v)

outdegree :: (Ix v) => Graph v -> Table v Int
outdegree g = mapT numEdges g
  where numEdges v ws = length ws

buildG :: (Ix v) => Bounds v -> [Edge v] -> Graph v
buildG bnds es = accumArray (flip (:)) [] bnds es

transposeG :: (Ix v) => Graph v -> Graph v
transposeG g = buildG (bounds g) (reverseE g)

reverseE :: (Ix v) => Graph v -> [Edge v]
reverseE g = [ (w,v) | (v,w) <- edges g ]

indegree :: (Ix v) => Graph v -> Table v Int
indegree = outdegree . transposeG

data Tree a = Node a (Forest a) deriving (Show)
type Forest a = [Tree a]

dff :: (Ix v) => Graph v -> Forest v
dff g = dfs g (vertices g)

preorder :: Tree a -> [a]
preorder (Node a ts) = a : preorderF ts

preorderF :: Forest a -> [a]
preorderF ts = concatMap preorder ts

preOrd :: (Ix v) => Graph v -> [v]
preOrd g = preorderF (dff g)

tabulate :: (Ix v) => Bounds v -> [v] -> Table v Int
tabulate bnds vs = array bnds (zip vs [1..])

preArr :: (Ix v) => Bounds v -> Forest v -> Table v Int
preArr bnds ts = tabulate bnds (preorderF ts)

postorder :: Tree a -> [a]
postorder (Node a ts) = postorderF ts ++ [a]

postorderF :: Forest a -> [a]
postorderF ts = concatMap postorder ts

postOrd :: (Ix v) => Graph v -> [v]
postOrd g = postorderF (dff g)

topSort :: (Ix v) => Graph v -> [v]
topSort g = reverse (postOrd g)

components :: (Ix v) => Graph v -> Forest v
components g = dff (undirected g)

undirected :: (Ix v) => Graph v -> Graph v
undirected g = buildG (bounds g) (edges g ++ reverseE g)

scc :: (Ix v) => Graph v -> [[v]]
scc g = map preorder (dfs (transposeG g) (reverse (postOrd g)))

--scc' :: (Ix v) => Graph v -> Forest v
--scc' g = dfs g (reverse (postOrd (transposeG g)))

reachable :: (Ix v) => Graph v -> v -> [v]
reachable g v = preorderF (dfs g [v])

path :: (Ix v) => Graph v -> v -> v -> Bool
path g v w = w `elem` reachable g v

-----

generate :: (Ix v) => Graph v -> v -> Tree v
generate g v = Node v (map (generate g) (g!v))

type Set v s = MutArray s v Bool

mkEmpty :: (Ix v) => Bounds v -> ST s (Set v s)
mkEmpty bnds = newMutArray bnds False

contains :: (Ix v) => Set v s -> v -> ST s Bool
contains m v = readMutArray m v

include :: (Ix v) => Set v s -> v -> ST s ()
include m v = writeMutArray m v True

prune :: (Ix v) => Bounds v -> Forest v -> Forest v
prune bnds ts = runST (RunST (mkEmpty bnds >>= \ m -> chop m ts))

chop :: (Ix v) => Set v s -> Forest v -> ST s (Forest v)
chop m [] = return []
chop m (Node v ts : us) =
    do
	visited <- contains m v
	if visited then
	    chop m us
	 else
	    do
	        include m v
		as <- chop m ts
		bs <- chop m us
		return (Node v as : bs)

dfs :: (Ix v) => Graph v -> [v] -> Forest v
dfs g vs = prune (bounds g) (map (generate g) vs)

