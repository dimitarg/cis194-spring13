scale :: Floating a => Integer -> a
scale a = logBase 10 $ fromIntegral a

intScale :: Integer -> Integer
intScale = truncate . scale

firstDigit :: Integer -> (Integer, Integer)
firstDigit a =
  let
     exp        = scale a
     f          = snd $ properFraction $ exp
     firstDigit = truncate $ 10 ** f
     remaining  = a - firstDigit * (10 ^ truncate exp)
  in (firstDigit, remaining)


isOneDigit :: Integer -> Bool
isOneDigit a = a >= 0 && a <= 9

toDigits :: Integer -> [Integer]
toDigits a
  | a < 0           = []
  | isOneDigit a     = [a]
  | otherwise        =
    let (first, remaining) = firstDigit a
        nextIsZero         = ((intScale a) - (intScale remaining)) > 1
    in
      if nextIsZero then first:0:(toDigits remaining) else first:(toDigits remaining)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs =
  let doubleEveryOther_ :: Integer -> [Integer] -> [Integer]
      -- TODO revise with foldr
      doubleEveryOther_ _ [] =
        []
      doubleEveryOther_ i (a:as)
        | i `mod` 2 == 1 = (a*2):(doubleEveryOther_ (i+1) as)
        | otherwise      = a:(doubleEveryOther_ (i+1) as)
  in  reverse $ doubleEveryOther_ 0 $ reverse xs


sumDigitsI :: Integer -> Integer
sumDigitsI a
  | a < 0        = sumDigitsI (-a)
  | isOneDigit a = a
  | otherwise    =
    let (first, remaining) = firstDigit a
    in                       first + sumDigitsI remaining

sumDigits :: [Integer] -> Integer
sumDigits as =
  foldr (\a acc -> acc + (sumDigitsI a)) 0 as

validate :: Integer -> Bool
validate a =
  let digits  = toDigits a
      doubled = doubleEveryOther digits
      summed  = sumDigits doubled
  in
    (summed `mod` 10) == 0

-- 4012 8888 8888 1881
