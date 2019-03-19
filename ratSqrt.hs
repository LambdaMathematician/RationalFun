import Test.QuickCheck
import Data.Ratio
import Data.List.Split

type Eps = Rational
type Xn = Rational
type Xn' = Rational

eps=1e-16::Eps


-- Newton's method for approximating a square root
xn' :: Rational -> Xn -> Xn'
xn' radicand xn = (xn + radicand/xn)/2

-- Do newton's method until you get within the error.
--ratSqrt 0 _ = [0]
--ratSqrt x eps = takeWhile (not.withinError) $ iterate f init
ratSqrt :: Rational -> Eps -> Rational
ratSqrt 0 _ = 0
ratSqrt radicand eps = until withinError f init
  where
    withinError a = squaresError radicand a < eps^2
    f::Rational -> Rational
    f = (flip trimRat (eps^2)).(xn' radicand)
    init = get_x0 radicand

-- guesses one more than the integer square root. This 
-- guarantees that the initial guess is nonzero, which
-- would break x_nplus1
get_x0 :: Rational -> Rational
get_x0 x = 1 + (toRational $ intSqrt $ floor $ fromRational x)

prop_getx0 x = (x>0) ==> ((a-1)^2 <= x && x <= a^2)
  where
    a = get_x0 x

squaresError :: Rational -> Rational -> Rational
squaresError radicand approximateSqrt = abs (radicand - approximateSqrt^2)

prop_ratSqrt :: Rational -> Property
prop_ratSqrt x = (x >= 0) ==> (abs (x - (ratSqrt x eps)^2) <= eps^2 )

-- Check to see if it aligns with the built-in square root function
-- This fails from time to time, mostly because the haskell (c) sqrt
-- is bad. 18%1 and 257%1 for example will fail.
prop_eq_sqrt :: Rational -> Property
prop_eq_sqrt x = (x >= 0)  ==> abs( haskSqrt - mySqrt) < (fromRational eps::Double)
  where
    haskSqrt = (sqrt $ fromRational x)::Double
    mySqrt = (fromRational $ ratSqrt x eps)::Double

mapT f (x,y) = (f x, f y)

-- Right shifts in base 10
trimInteger :: Integer -> Integer
trimInteger x = (signum x) * (div (abs x) 10)

--this one appears slower but I'm leaving it here for funsies
--(averaged about 30% longer to run)
trimInteger2 :: Integer -> Integer
trimInteger2 x
  | abs x < 10 = 0
  | otherwise = read (init $ show x)::Integer

trimRat' :: (Integer, Integer) -> [(Integer, Integer)]
trimRat' (a,b) = takeWhile denominatorNonZero $ trimList
  where
    denominatorNonZero (_, m) = m /= 0
    trimList = iterate (mapT trimInteger) (a,b)

trimRat :: Rational -> Eps -> Rational
trimRat x eps = n%d
  where
    (n,d) = last $ takeWhile withinError $ trimRat' (numerator x, denominator x)
    withinError (a,b) = abs (a%b - x) < eps

prop_trim x = x - (trimRat x (eps)) < eps

prop_b1 x = trimInteger x == trimInteger2 x

dumbRat x = x::Rational

deepCheck prop num = quickCheckWith (stdArgs {maxSuccess = num}) prop
deepVerboseCheck prop num = verboseCheckWith (stdArgs {maxSuccess = num}) prop

-- Integer sqrt stolen from haskell wiki
-- https://wiki.haskell.org/Generic_number_type#squareRoot
(^!) :: Num a => a -> Int -> a
(^!) x n = x^n

intSqrt :: Integer -> Integer
intSqrt 0 = 0
intSqrt 1 = 1
intSqrt n =
   let twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (intSqrt (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
   in  head $ dropWhile (not . isRoot) iters


