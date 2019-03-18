x_nplus1 :: Rational -> Rational -> Rational
x_nplus1 radicand xn = 0.5 * (xn + radicand/xn)

it :: Rational -> (Rational, Rational) -> (Rational, Rational)
it radicand (xn, error) = (xnn, errorn)
  where
    xnn = x_nplus1 radicand xn 
    errorn
      | error < 0 && (xnn - 1)^2 < radicand && radicand < (xnn + 1 )^2 = toRational 1
      | error < 0 = error
      | otherwise = error^2/(2 * xn)

mysqrt :: Rational -> Rational -> (Rational, Rational)
mysqrt x errorReq = until (\(_,a) -> (a >= 0 && a < errorReq)) (it x) (initialGuess x, -1)

initialGuess :: Rational -> Rational
initialGuess x = toRational $ squareRoot $ floor $ fromRational x

mapT :: (a -> b) -> (a, a) -> (b, b)
mapT f (a1, a2) = (f a1, f a2)

-- Error bound check
squaredErrorCheck :: Rational -> Rational -> Bool
squaredErrorCheck radicand errorReq = (abs (radicand - approxRoot^2)) <= errorReq^2
  where
    (approxRoot, _ ) = mysqrt radicand errorReq
    squaresError = radicand - approxRoot





-- Stolen from haskell wiki https://wiki.haskell.org/Generic_number_type#squareRoot
(^!) :: Num a => a -> Int -> a
(^!) x n = x^n

squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
   in  head $ dropWhile (not . isRoot) iters
