module P_List_reverse(reverse)  where
reverse		:: [a] -> [a]
reverse l	=  rev l []
rev []     a = a
rev (x:xs) a = rev xs (x:a)
