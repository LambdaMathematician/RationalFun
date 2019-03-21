import Test.QuickCheck
import Data.Ratio
import Prelude hiding ( pi )

pi' = 31415926535897932384626433832795028841 % 10^37

macLaurinSinTerms :: Rational -> [Rational]
macLaurinSinTerms x = map makeTerms [0..]
  where
    makeTerms n = (-1)^n*x^^(2*n+1)/(fromInteger $ factorial (2*n + 1))

factorial :: Integer -> Integer
factorial n
  | n < 0 = error "STAHP. Go extend Eulerian numbers or something"
  | n == 0 = 1
  | otherwise = n * (factorial (n-1))

macLaurinSin' terms x = sum $ take terms $ macLaurinSinTerms x

macLaurinSin :: Rational -> Rational -> Rational
macLaurinSin x error
  | otherwise = sum $ takeWhile (\t -> (abs t) > error) $ macLaurinSinTerms normx
  where
    normx = putBetween0and2pi x

putBetween0and2pi :: Rational -> Rational
putBetween0and2pi x
  | x >= 0 && x < 2*pi' = x
  | otherwise = x - n*2*pi'
    where n = (div (numerator x * denominator pi') (2*denominator x * numerator pi') % 1)

prop_between x = (result >= 0 && result < 2*pi')
  where
    result = putBetween0and2pi x

--prop_mysin x = abs (sin x - (fromRational $ macLaurinSin (toRational x) 1e-20)) < x*1e-16

--between0andpi = choose (0::Double, 3.141592653589793::Double)

--prop_small = forAll between0andpi (prop_mysin)
