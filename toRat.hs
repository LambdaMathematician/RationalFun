import Test.QuickCheck
import Data.Ratio
import Data.List.Split

-- toRational has some weird behavior... Might have to roll my own toRational function...
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

deepCheck prop num = quickCheckWith (stdArgs {maxSuccess = num}) prop
