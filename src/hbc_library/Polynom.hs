module Polynomial(Polynomial, degree, leadingCoeff, mkPolynomial, getCoeff, scalarMul, scaleBy, evaluate, divide) where
import Algebra
import QSort

-- Equality on the elements is a pain, but it makes it possible to avoid zero coefficients.
data (Ring a, Eq a) => Polynomial a =
	P [(a, Int)]  -- coefficient, exponent pairs.  Ordered for descending exponent. All coeffs /= zero
	deriving (Eq)

-- Only rings with unity can be printed because of the test for one.
instance (Text a, UnityRing a, Eq a) => Text (Polynomial a) where
    showsType x = showString "(Polynomial " . showsType x . showString ")"
    showsPrec p (P l@[]) = showsPrec p (zero `asTypeOf` (fst (head l)))
    showsPrec p (P as) = showParen (p>6) (showString (f as)) 
			where f [(a,n)] = g a n
			      f ((a,n):as) = g a n ++ " + " ++ f as
			      g a 0 = show a
			      g a 1 = if a == one then "x" else showsPrec 7 a "*x"
			      g a n = if a == one then "x^"++show n else showsPrec 7 a ("*x^"++show n)

-- Highest exponent
degree :: (Ring a, Eq a) => Polynom a -> Int
degree (P []) = 0
degree (P ((_,n):_)) = n

leadingCoeff :: (Ring a, Eq a) => Polynom a -> Int
leadingCoeff (P []) = zero
leadingCoeff (P ((a,_):_)) = a

-- Make a polymonial from an unsorted list with possibly duplicated exponents.
mkPolynomial :: (Ring a, Eq a) => [(a, Int)] -> Polynomial a
mkPolynomial ans = P (sumUp (sortLe (\(_,n)-> \(_,m)->n>=m) ans))
	where sumUp (an@(a,n):bn@(b,n'):xs) | n==n'     = sumUp ((a+.b,n):xs)
				            | otherwise = an:sumUp (bn:xs)
	      sumUp xs = xs

-- Get a coefficient for a particular exponent
getCoeff :: (Ring a, Eq a) => Polynom a -> Int -> a
getCoeff (P ans) k = f ans where f ((a,n):ans) | k > n = zero
					       | k < n = f ans
					       | otherwise = a
--x :: (UnityRing a, Eq a) => Polynomial a
--x = P [(one, 1)]
--k c = P [(c, 0)]

instance (Ring a, Eq a) => SemiGroup (Polynomial a) where
    P as +. P bs = P (f as bs)
        where f [] b  = b
	      f a  [] = a
	      f aas@((a,n):as) bbs@((b,m):bs) | n<m  = (b,m) : f aas bs
                                              | n>m  = (a,n) : f as bbs
					      | otherwise = 
					              let ab = a+.b
						      in  if ab==zero then
						              f as bs
						          else
						              (ab,n) : f as bs
instance (Ring a, Eq a) => Monoid (Polynomial a) where
    zero = P []
instance (Ring a, Eq a) => Group (Polynomial a) where
    neg (P as) = P [(neg x,n) | (x,n)<-as]
instance (Ring a, Eq a) => AbelianGroup (Polynomial a)

-- Multiply a polymonial by a value from the ring.
scalarMul :: (Ring a, Eq a) => a -> Polynom a -> Polynom a
scalarMul s (P as) = P [(sa,n) | (a,n)<-as, sa<-[s*.a], sa /= zero]
-- Multiply a polymonial by x^m
scaleBy :: (Ring a, Eq a) => Int -> Polynom a -> Polynom a
scaleBy m (P as) = P [(a,n+m) | (a,n)<-as]

instance (Ring a, Eq a) => Ring (Polynomial a) where
    P [] *. _ = P []
    P ((a,n):as) *. b = scaleBy n (scalarMul a b) +. P as *. b

instance (UnityRing a, Eq a) => UnityRing (Polynomial a) where
    one = P [(one,0)]

instance (CommutativeRing a, Eq a) => CommutativeRing (Polynomial a)

instance (IntegralDomain a, Eq a) => IntegralDomain (Polynomial a)

-- The evaluation homomorphism, usually for a field, but 
-- only a commutative ring with unity is needed.
evaluate :: (Eq a, CommutativeRing a, UnityRing a) => Polynomial a -> a -> a
evaluate (P []) x = zero
evaluate (P ((a,n):as)) x = f n as a
		where f 0 [] r = r
		      f k [] r = f (k-1) [] (r*.x)
		      f k aas@((a,n):as) r | k > n     = f (k-1) aas (r*.x)
		                           | otherwise = f k     as  (r+.a)

-- Polynomial division works in any field.
divide :: (Eq a, Field a) => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
divide _    (P []) = error "Polynomial divide by zero"
divide (P a) bs@(P ((b,m):_)) = let (q, r) = f a in (P q, P r)
	where f [] = ([], [])
              f as@((a,n):ans) | n < m = ([], as)
			       | otherwise =
		let c = a/.b  -- coeff of first term
	    	    k = n-m   -- exponent of first term
		    (P d) = P as -. scaleBy k (scalarMul c bs)
		    (q, r) = f d
		in  ((c,k):q, r)
