module P_List_concat  where
concat			:: [[a]] -> [a]
concat []		= []
concat ([]:xss)		= concat xss			-- for better stack behaviour!
concat (xs:xss)		= xs ++ concat xss
