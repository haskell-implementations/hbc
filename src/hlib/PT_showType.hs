module P_Text_showType where
showType	:: (Text a) => a -> String
showType x	= showsType x ""

