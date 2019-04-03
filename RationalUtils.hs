module RationalUtils where

import Test.QuickCheck
import Data.Ratio
import Data.List.Split

type Eps = Rational

-- I'd like to calculate the cirumference of the known universe
-- to within one hydrogen atom of error please
-- https://www.jpl.nasa.gov/edu/news/2016/3/16/how-many-decimals-of-pi-do-we-really-need/
pi' = 31415926535897932384626433832795028841972 % 10^40
eps=2.23e-16::Eps

-- toRational has some weird behavior... 
-- Might have to roll my own toRational function.
toRat :: (Show a, RealFrac a) => a -> Rational
toRat x = allDigits % denom * 10^^exponent
  where
    digits:e = splitOn "e" (show x)
    whole:dec = splitOn "." digits
    allDigits = read (whole ++ head dec)::Integer
    denom = 10^(toInteger $ length $ head dec)
    exponent
      | length e == 0 = 0::Integer
      | otherwise     = read (head e)::Integer

prop_rat :: (Show a, RealFrac a) => a -> Bool
prop_rat x = x == fromRational (toRat x)

mapT f (x,y) = (f x, f y)

-- right  shifts in base 10
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
prop_trim2 x = trimInteger x === trimInteger2 x

factorial :: Integer -> Integer
factorial n
  | n < 0 = error "STAHP. Go extend Eulerian numbers or something"
  | otherwise = product [1..n]

nChoosek' n k = div (product [1..n]) ((product [1..k])*(product [1..(n-k)]))

nChoosek n k = div (product [mm..n]) (product [1..m])
  where
    mm = 1+ max k (n-k)
    m = min k (n-k)

prop_nck (n,k) = n >=k && k >= 0 ==> nChoosek' n k == nChoosek n k

kroneckerDelta :: (Integer, Integer) -> Integer
kroneckerDelta (i, j)
  | i /= j = 0
  | otherwise = 1

--This is using the recursive definition to calculate a specific term
dumbbernoulli :: Integer -> Rational
dumbbernoulli m
  | not $ oneOrEven m = 0
  | otherwise = ((kroneckerDelta (m,0))%1) - recursiveSum
  where
    recursiveSum = sum $ map makeTerm $ filter oneOrEven [0..(m-1)]
    oneOrEven k = k == 1 || even k
    makeTerm k = (nChoosek m k)%(m-k+1) * (dumbbernoulli k)


--adds to the list so it doesn't have to recalculate for every sum
bernoullis :: [Rational]
bernoullis = map last $ iterate appendNextBernoulli [1]

appendNextBernoulli :: [Rational] -> [Rational]
appendNextBernoulli bs = bs ++ [makeNextBernoulli bs]

makeNextBernoulli :: [Rational] -> Rational
makeNextBernoulli bs = (kroneckerDelta (m,0))%1 - recursiveSum
  where
    m = toInteger $ length bs
    recursiveSum = dotprod coeffsList bs
    coeffsList = map makecoeff [0..(m-1)]
    makecoeff k = (nChoosek m k)%(m-k+1)

oddBernoullisAreZero n = and $ take (div n 2) $ map (==0) $ map fst $ drop 1 $ filter (\(_,k) -> odd k) $ zip bernoullis [0..]

prop_oddBsGT1AreZero n = (n > 0) ==> bernoullis !! (2*n+1) == 0


-- why is this consisntenly a little slower?!?!
-- (:) is faster than ++, and head
-- is faster than last. Surely it's not the index inversion...
maybeOptobern = map head $ iterate addBernoulliToHead [1]

addBernoulliToHead bs
  | m > 2 && odd m = 0%1:bs -- increases efficiency by a lot
  | otherwise = (makeNextBernoulliHead bs) : bs
  where
    makeNextBernoulliHead bs = (kroneckerDelta (m,0))%1 - sums
    m = toInteger $ length bs
    sums = dotprod coeffs bs
    coeffs = map (\k -> (nChoosek m k)%(m-k+1)) $ map (m-1-) [0..(m-1)]

--pretty efficient algorithm from rosetta code but i don't understand it
--also this makes B(1) = 0.5 (as opposed to the standard -0.5)
--ACTUALLY I WIN BY LIKE 60% NOW
rcbernoulli = map head . iterate (ulli 1) . map berno $ enumFrom 0
berno i = 1 % (i + 1)
ulli _ [_] = []
ulli i (x:y:xs) = (i % 1) * (x - y) : ulli (i + 1) (y : xs)

dotprod xs ys = sum $ zipWith (*) xs ys

roundXtoNdigits x n = (fromInteger $ round $ x * (10^n)) / (10.0^^n)

deepCheck prop num = quickCheckWith (stdArgs {maxSuccess = num}) prop
deepVerboseCheck prop num = verboseCheckWith (stdArgs {maxSuccess = num}) prop

