import Test.QuickCheck
import Data.Ratio
import Prelude hiding ( pi )

pi' = 31415926535897932384626433832795028841 % 10^37

macLaurinSinTerms :: Rational -> [Rational]
macLaurinSinTerms x = map f [0..]
  where
    f n = (-1)^n*x^^(2*n+1)/(fromInteger $ factorial (2*n + 1))

factorial :: Integer -> Integer
factorial n
  | n < 0 = error "stahp"
  | n == 0 = 1
  | otherwise = n * (factorial (n-1))

macLaurinSin' terms x = sum $ take terms $ macLaurinSinTerms x

macLaurinSin :: Rational -> Rational -> Rational
macLaurinSin x error
  | x < 0 = (-1)* macLaurinSin (-x) error
  | x > 2*pi' = macLaurinSin (x - 2*pi') error
  | otherwise = sum $ takeWhile (\t -> (abs t) > error) $ macLaurinSinTerms x

putBetween0and2pi x
  | x < 0 = putBetween0and2pi (-x)
  | x > 2*pi' = x - n*pi'
--  | otherwise = x
    where n = (div (numerator x * numerator pi') (denominator x * denominator pi')) % 1

--prop_mysin x = abs (sin x - (fromRational $ macLaurinSin (toRational x) 1e-20)) < x*1e-16

--between0andpi = choose (0::Double, 3.141592653589793::Double)

--prop_small = forAll between0andpi (prop_mysin)
