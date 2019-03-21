module RatSqrt where

import Test.QuickCheck
import Data.Ratio
import Data.List.Split
import RationalUtils

type Xn = Rational
type Xn' = Rational

-- Newton's method for approximating a square root
xn' :: Rational -> Xn -> Xn'
xn' radicand xn = (xn + radicand/xn)/2

-- Since x^2-S=0 is a really nice function, we can use theoretical errob bounds
-- See equation (6) at https://en.wikipedia.org/wiki/Newton%27s_method
-- section #Proof_of_quadratic_convergence_for_Newton's_iterative_method
xn'wError :: Rational -> (Rational, Eps) -> (Rational, Eps)
xn'wError radicand (xn, en) = (xnn, enn)
  where
    xnn = xn' radicand xn
    enn = en^2 / (2 * xn)

ratSqrt 0 _ = 0 
--newtonSqrt radicand eps = take 5 $ iterate (xn'wError radicand) (get_x0 radicand, 1)
--newtonSqrt radicand eps = until withinError (xn'wError radicand) (get_x0 radicand, 1)
ratSqrt radicand eps = fst $ until withinError iterateAndTrim (get_x0 radicand, 1)
  where
    withinError (_,e) = e < eps
    iterateAndTrim = trimTuple . iterateNewton
    iterateNewton = xn'wError radicand
    trimTuple = mapT $ flip trimRat (eps*1e-18)

-- Makes initial approximation using integer square root. max
-- guarantees that the initial guess is nonzero, which
-- would break xn'
get_x0 :: Rational -> Rational
get_x0 x = max 1 (toRational $ intSqrt $ floor $ fromRational x)

-- Check to see if it's at least as good as the built-in square root function
prop_alaga_sqrt :: Rational -> Property
prop_alaga_sqrt x = (x >= 0)  ==> abs( x - mySqrt^2) <= abs ( x - haskSqrt^2)
  where
    haskSqrt = toRational $ sqrt (fromRational x::Double)
    mySqrt = ratSqrt x (x/1e18)

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

-- end of stolen code
prop_intSqrt n = (n >= 0) ==> (intSqrt n)^2 <= n && n < (1 + intSqrt n)^2
