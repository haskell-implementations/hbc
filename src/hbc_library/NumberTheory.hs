module NumberTheory(factorial, binCoeff, primes, factorize, igcd,
                    (%%), modAdd, modSub, modMul, modDiv, modPow, modOps,
                    isPrime) where

import Trace

import QSort
import IntegerMisc
infix 7 %%
default (Integer)

-- Finally, a legitimate use of fac!
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

-- Binomial coefficients
binCoeff :: Integer -> Integer -> Integer
binCoeff n k = product [n-k+1 .. n] `quot` factorial k

-- Prime numbers
primes :: [Integer]
primes = map fromInt (sieve [2..])
		where sieve (p:xs) = (p::Int) : sieve [x | x<-xs, x `rem` p /= 0]

-- Prime factorization, really slow
factorize :: Integer -> [Integer]
factorize n = f primes		-- trial division first
		where f (p:ps) = if p*p > n then [n] 
				 else if n `rem` p == 0 then p : factorize (n `quot` p)
				 else if p > maxTest then sort (factorize' n)
				 else f ps
-- Trial division has found all small factors, use Fermats algorithm to find the biggest
factorize' :: Integer -> [Integer]
#if 0
factorize' n =
	let (sn,r0) = integerSqrt n
	    x' = 2*sn+1		-- 2x+1
	    y' = 1		-- 2y+1
	    r = -r0		-- x^2-y^2-n
	    done (x',y',r) = r == 0
	    step (x',y',r) = trace ("ostep "++show(x',y',r)) (until1 (\(x',y',r)->r<=0) 
	                            (\(x',y',r)->trace ("istep "++show(x',y',r)) (x',y'+2,r-y'))
				    (x'+2,y',r+x'))
	    (x,y,_) = until done step (x',y',r)
	    f = (x-y) `quot` 2	-- largest factor <= sqrt(n)
	    m = (x+y+2) `quot` 2
	in  trace ("start "++show[sn,r0,x',y',r]) ({-factorize' m ++-} [m,f])

until1 f g x = until f g (g x)
#else
factorize' = error "factorize'"
#endif

-- Greatest common divisor
igcd :: Integer -> Integer -> Integer
igcd x y = integerGcd x y

-- Modulo arithmetic
(%%) :: Integer -> Integer -> Integer
x %% y =
	let r = x `rem` y
	in  if r < 0 then r+y else r
modAdd :: Integer -> Integer -> Integer -> Integer
modAdd n x y = (x+y) %% n
modSub :: Integer -> Integer -> Integer -> Integer
modSub n x y = (x-y) %% n
modMul :: Integer -> Integer -> Integer -> Integer
modMul n x y = (x*y) %% n
modDiv :: Integer -> Integer -> Integer -> Integer
modDiv n x y = (x `div` y) %% n
modPow :: Integer -> Integer -> Integer -> Integer
modPow n x y = integerPowMod x y n

-- Do 
--    let ((+),(-),(*),(/),(^), mod) = modOps N;;
-- to get modular arithmetic in hbi.
modOps n = (modAdd n, modSub n, modMul n, modDiv n, modPow n, (%% n))

-- Primality tester
maxTest :: Integer
maxTest = 10000			-- limit for naive test
isPrime :: Integer -> Bool
isPrime n = f primes		-- algorithm: trial division 
		where f (p:ps) = if p > maxTest then 
					isPrime' n
				 else
				        p*p > n || n `rem` p /= 0 && f ps

isPrime' x = error "isPrime'"
-- log

-- phi
