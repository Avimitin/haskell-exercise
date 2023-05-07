toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n
  | n < 0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

doubleEveryOtherCalc :: [Integer] -> [Integer]
doubleEveryOtherCalc [] = []
doubleEveryOtherCalc [x] = [x]
doubleEveryOtherCalc (x1 : x2 : xs) = x1 : x2 * 2 : doubleEveryOtherCalc xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = reverse (doubleEveryOtherCalc (reverse list))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs)
  | x < 10 = x + sumDigits xs
  | otherwise = sumDigits (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate x = remainder == 0
  where
    bankDigits = toDigits x
    doubledDigits = doubleEveryOther bankDigits
    sum = sumDigits doubledDigits
    remainder = sum `mod` 10