module NameSupply(NameSupply, initialNameSupply, splitNameSupply, getName, listNameSupply, listName, Name(..)) where
import LMLgensym

type Name = Int
data NameSupply = NameSupply Name NameSupply NameSupply

initialNameSupply :: IO NameSupply
initialNameSupply = return (gen ())
	where gen n = NameSupply (__gensym n) (gen n) (gen n)

splitNameSupply :: NameSupply -> (NameSupply,NameSupply)
splitNameSupply (NameSupply _ s1 s2) = (s1, s2)

getName :: NameSupply -> Name
getName (NameSupply k _ _) = k

listNameSupply :: NameSupply -> [NameSupply]
listNameSupply (NameSupply _ s1 s2) = s1 : listNameSupply s2

listName :: NameSupply -> [Name]
listName (NameSupply k _ s2) = k : listName s2
