let rec
   cnc [] a = a
|| cnc (a.b) c = a.cnc b c
in
let
itos n = (if n < 0 then '-'.itos1 (-n)
		   else itos1 n
		where rec itos1 n =
			if n < 10 then [chr (n + ord '0')]
				  else cnc (itos1 (n/10)) [chr (n%10+ord '0')])
in
let rec fib n = 
	if n < 2 then
		1
	else
		fib(n-1) + fib(n-2) + 1
in cnc (itos (fib 5)) "\n"
