module Fuel where

main :: IO Int
main = do
  inputNumbers <- readFile "day1inputs.txt"

  let fuelAmountList = parseInput . lines $ inputNumbers
      fuelMass = foldl1 (+) . map moduleFuel $ fuelAmountList
    in return fuelMass
  
parseInput :: [String] -> [Int]
parseInput [] = []
parseInput (x:xs) =
  read x : parseInput xs

moduleFuel :: Int -> Int
moduleFuel mass
  | mass > 0  = if newMass > 0 then newMass + moduleFuel newMass else 0
  | otherwise = 0
  where newMass = div mass 3 - 2
