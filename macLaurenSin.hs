import Test.QuickCheck
import Data.Ratio
import RationalUtils

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

-- Calculate the sine of a value to a given precision using
-- MacLaurin series. 
macLaurinSin :: Rational -> Rational -> Rational
macLaurinSin x eps
  | normx <= pi'/2 = sum $ takeWhile (\t -> (abs t) > eps) $ macLaurinSinTerms normx
  | normx <= pi'    = macLaurinSin (pi' - normx) eps
  | otherwise      = -macLaurinSin (normx - pi') eps
  where
    normx = niceRangeForSin x

normalize :: Rational -> Rational
normalize x
  | x >= 0 && x < 2*pi' = x
  | otherwise = x - n*2*pi'
    where n = (div (numerator x * denominator pi') (2*denominator x * numerator pi') % 1)

niceRangeForSin x
  | normx > 3*pi'/2 = normx - 2*pi'
  | otherwise = normx
  where
    normx = normalize x

prop_rangeCheck x = (result >= -pi'/2 && result < 3/2*pi')
  where
    result = niceRangeForSin x

prop_mysin :: Rational -> Bool
prop_mysin x = abs((toRational $ sin $ fromRational x) - (macLaurinSin (toRational $ fromRational x) eps)) < 2*eps

xs=take 200 $ iterate (+pi'/100) 0
takeSins = map (flip macLaurinSin eps) xs

--prop_mysin x = abs (sin x - (fromRational $ macLaurinSin (toRational x) 1e-20)) < x*1e-16

--between0and2pi = choose (0,2*pi')

arbitraryDice :: Gen (Int,Int)
arbitraryDice = arbitrary::Gen (Int, Int) 



--prop_small = forAll between0andpi (prop_mysin)
