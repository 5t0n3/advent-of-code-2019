module Passwords where

import Data.Char
import Data.List

part1 :: IO Int
part1 = do
  rawInput <- readFile "day4input.txt"
  let passRange = parseRange rawInput
      nonDecreasingPasswords = filter allDigitsNotDescending passRange
      samePairPasswords = filter anySameDigitPairs passRange
  return (length $ intersect nonDecreasingPasswords samePairPasswords)

part2 :: IO Int
part2 = do
  rawInput <- readFile "day4input.txt"
  let passRange = parseRange rawInput
      nonDecreasingPasswords = filter allDigitsNotDescending passRange
      samePairPasswords = filter anySameDigitPairs passRange
      part1passwords = intersect nonDecreasingPasswords samePairPasswords
  return $ length (filter (not . null . getPairDigits) part1passwords)

test :: IO ()
test = do
  part1res <- part1
  if part1res /= 1748
    then putStrLn ("Part 1 failed with result: " ++ show part1res)
    else putStrLn "Part 1 succeeded."
  part2res <- part2
  if part2res /= 1180
    then putStrLn ("Part 2 failed with result: " ++ show part2res)
    else putStrLn "Part 2 succeeded."
  
pairNotDescending :: (Int, Int) -> Bool
pairNotDescending (digit1, digit2) = digit1 <= digit2

allDigitsNotDescending :: Int -> Bool
allDigitsNotDescending num =
  all (\pair -> pairNotDescending pair) pairs
  where numStr = show num
        pairs = adjacentPairs numStr

anySameDigitPairs :: Int -> Bool
anySameDigitPairs num =
  any (\(first,second) -> first == second) pairs
  where numStr = show num
        pairs = adjacentPairs numStr

-- Returns each digit as well as the triple's beginning index
getPairDigits :: Int -> [(Int,[Int])]
getPairDigits num
  | not $ anySameDigitPairs num = []
  | otherwise = onlyPairDigits
  where pairs = adjacentPairs $ show num
        twoOfAKind = \(first,second) -> first == second
        pairDigits = nub . filter twoOfAKind $ pairs
        digitsWithIndices = zip (map fst pairDigits)
                            (map (\pair -> findIndices (==pair) pairs) pairDigits)
        onlyPairDigits = filter (not . null . snd) . removeIfNotPairs $ digitsWithIndices

removeIfNotPairs :: [(Int,[Int])] -> [(Int,[Int])]
removeIfNotPairs = map (\(digit,is) -> (digit,onlyPairs is))
    where onlyPairs indices = filter (not . moreThan2Adjacent indices) indices

moreThan2Adjacent :: [Int] -> Int -> Bool
moreThan2Adjacent list basenum = any (inRange basenum) (removeOneFromList basenum list)
  where inRange base comp = base - 2 <= comp && comp <= base + 2

adjacentPairs :: String -> [(Int, Int)]
adjacentPairs numString
  | length numString < 2 = []
  | otherwise = (digitToInt first, digitToInt second) : adjacentPairs (second:rest)
  where (first:second:rest) = numString

parseRange :: String -> [Int]
parseRange stringRange =
  [(read first)..(read second)]
  where (first, _:second) = break (== '-') stringRange

removeOneFromList :: Eq a => a -> [a] -> [a]
removeOneFromList _ [] = []
removeOneFromList item (x:xs)
  | x == item = xs
  | otherwise = x : removeOneFromList item xs
 
