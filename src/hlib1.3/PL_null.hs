module PreludeX where
null			:: [a] -> Bool
null []			=  True
null (_:_)		=  False
