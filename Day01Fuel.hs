module Day01Fuel where

part1 :: IO Int
part1 = do
  input <- readFile "input/day01.txt"
  let fuelAmountList = map read . lines $ input
      fuelMass = sum . map moduleFuelNaive $ fuelAmountList
  return fuelMass

part2 :: IO Int
part2 = do
  input <- readFile "input/day01.txt"
  let fuelAmountList = map read . lines $ input
      fuelMass = sum . map moduleFuel $ fuelAmountList
  return fuelMass

moduleFuelNaive :: Int -> Int
moduleFuelNaive mass = div mass 3 - 2

moduleFuel :: Int -> Int
moduleFuel mass
  | mass > 0 && newMass > 0 = newMass + moduleFuel newMass
  | otherwise = 0
  where
    newMass = div mass 3 - 2
