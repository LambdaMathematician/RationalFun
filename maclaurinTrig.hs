import Test.QuickCheck
import Data.Ratio
import RationalUtils

maclaurinSinTerms :: Rational -> [Rational]
maclaurinSinTerms x = map makeTerms [0..]
  where
    makeTerms n = (-1)^n*x^^(2*n+1)/(fromInteger $ factorial (2*n + 1))

maclaurinSin' terms x = sum $ take terms $ maclaurinSinTerms x

-- Calculate the sine of a value to a given precision using
-- MacLaurin series. 
maclaurinSin :: Rational -> Rational -> Rational
maclaurinSin eps x
  | normx <= pi'/2 = sum $ takeWhile (\t -> (abs t) > eps) $ maclaurinSinTerms normx
  | normx <= pi'    = maclaurinSin eps (pi' - normx)
  | otherwise      = -maclaurinSin eps (normx - pi')
  where
    normx = niceRangeForSin x

maclaurinCosTerms :: Rational -> [Rational]
maclaurinCosTerms x = map makeTerms [0..]
  where
    makeTerms n = (-1)^n*x^^(2*n)/(fromInteger $ factorial (2*n))

maclaurinCos' terms x = sum $ take terms $ maclaurinCosTerms x

maclaurinCos eps x
  | normx <= pi'/2 = sum $ takeWhile (\t -> (abs t) > eps) $ maclaurinCosTerms normx
  | normx <= pi'   = -maclaurinCos eps (pi' - normx)
  | otherwise      = maclaurinCos eps (2*pi' - normx)
  where
    normx = normalize x

maclaurinTanTerms :: Rational -> [Rational]
maclaurinTanTerms x = map makeTerms [1..]
  where
    makeTerms :: Integer -> Rational
    makeTerms n = (b2n n)*(-4)^^n*(1-4^^n)*(1%(factorial (2*n)))*(x^(2*n-1))
    b2n n = bernoullis !! (fromInteger (2*n))

maclaurinTan eps x = sum $ takeWhile (\t -> (abs t) > eps) $ maclaurinTanTerms x

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
prop_mysin x = abs((toRational $ sin $ fromRational x) - (maclaurinSin eps (toRational $ fromRational x))) < 2*eps

prop_mycos :: Rational -> Bool
prop_mycos x = abs((toRational $ cos $ fromRational x) - (maclaurinCos eps (toRational $ fromRational x))) < 2*eps

prop_pythag x = abs( (maclaurinSin eps x)^2 + (maclaurinCos eps x)^2 - 1 + 2 * eps^2) < 4*eps*(1 + eps)

xs=take 200 $ iterate (+pi'/100) 0
takeSins = map (maclaurinSin eps) xs

--prop_mysin x = abs (sin x - (fromRational $ maclaurinSin (toRational x) 1e-20)) < x*1e-16

--between0and2pi = choose (0,2*pi')

arbitraryDice :: Gen (Int,Int)
arbitraryDice = arbitrary::Gen (Int, Int) 



--prop_small = forAll between0andpi (prop_mysin)
