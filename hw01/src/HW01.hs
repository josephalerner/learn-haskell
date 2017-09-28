module HW01 where
import Data.Char

toDigits :: Integer -> [Integer]
toDigits num = if (num <= 0) then []
  else (map (\c -> (charToInteger c)) (integerToString num))

integerToString :: Integer -> String
integerToString num = (show num)

charToInteger :: Char -> Integer
charToInteger c = toInteger (digitToInt c)

toDigitsRev :: Integer -> [Integer]
toDigitsRev num = reverse (toDigits num)

doubleEveryOther :: [Integer] -> [Integer]
-- reverse the list before doubling so that every second digit from the right is doubled.
-- otherwise, every second digit from the left would be doubled
doubleEveryOther nums = reverse (doubleEveryOtherAux (reverse (nums)))

doubleEveryOtherAux :: [Integer] -> [Integer]
doubleEveryOtherAux [] = []
doubleEveryOtherAux (x:[]) = [x]
doubleEveryOtherAux (x:y:xs) = x : y * 2 : doubleEveryOtherAux(xs)

sumDigits :: [Integer] -> Integer
sumDigits nums = foldl (+) 0 (toDigits(concatIntegerLists (nums)))

-- took this function from:
-- https://stackoverflow.com/questions/1918486/convert-list-of-integers-into-one-int-like-concat-in-haskell
-- is it generally OK to take helper functions like these and cite them? I'm supposing yes?
concatIntegerLists :: [Integer] -> Integer
concatIntegerLists = foldl addDigit 0
  where addDigit num d = 10 * num + d

validate :: Integer -> Bool
validate num = (sumDigits(doubleEveryOther (toDigits (num))) `mod` 10) == 0
