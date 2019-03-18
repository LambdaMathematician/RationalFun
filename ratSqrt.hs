import Test.QuickCheck
import Data.Ratio
import Data.List.Split

eps=1e-15::Rational

-- Newton's method for approximating a square root
x_nplus1 :: Rational -> Rational -> Rational
x_nplus1 radicand xn = (xn + radicand/xn)/2

-- Do newton's method until you get within the error.
--ratSqrt 0 _ = [0]
--ratSqrt x eps = takeWhile (not.withinError) $ iterate f init
ratSqrt :: Rational -> Rational -> Rational
ratSqrt 0 _ = 0
ratSqrt x eps = until withinError f init
  where
    withinError a = squaresError x a < eps^2
    f::Rational -> Rational
    f = (flip trimRat (eps^2)).(x_nplus1 x)
    init = initialGuess x
initialGuess :: Rational -> Rational
initialGuess x = 1 + (toRational $ intSqrt $ floor $ fromRational x)

squaresError :: Rational -> Rational -> Rational
squaresError radicand approximateSqrt = abs (radicand - approximateSqrt^2)

prop_ratSqrt :: Rational -> Property
prop_ratSqrt x = (x >= 0) ==> (abs (x - (ratSqrt x eps)^2) <= eps^2 )

-- Check to see if it aligns with the built-in square root function
-- This fails from time to time, mostly because the haskell (c) sqrt
-- is bad. 18%1 and 257%1 for example will fail.
prop_eq_sqrt :: Rational -> Property
prop_eq_sqrt x = (x >= 0)  ==> abs( haskSqrt - mySqrt) < eps
  where
    haskSqrt = toRational $ sqrt $ fromRational x 
    mySqrt = ratSqrt x eps

-- Might have to roll my own toRational function...
mapT f (x,y) = (f x, f y)


trimRat' :: (Integer, Integer) -> [(Integer, Integer)]
trimRat' (a,b)
  | b < 10 = [(a,b)]
  | otherwise = (a,b):trimRat' (div a 10, div b 10)

trimRat :: Rational -> Rational -> Rational
trimRat x eps = n%d
  where
    (n,d) = last $ takeWhile withinError $ trimRat' (numerator x, denominator x)
    withinError (a,b) = abs (a%b - x) < eps

prop_trim x = x - (trimRat x eps) < eps

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
