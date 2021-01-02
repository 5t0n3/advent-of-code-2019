module Day09Boost where

import qualified Intcode

part1 :: IO [Integer]
part1 = do
  rawInput <- readFile "input/day09.txt"
  let parsedInput = Intcode.parseNumericInput rawInput
      (Intcode.Finished output _) = Intcode.executeIntcode $ Intcode.Executing 0 0 parsedInput [1] []
  return output

part2 :: IO [Integer]
part2 = do
  rawInput <- readFile "input/day09.txt"
  let parsedInput = Intcode.parseNumericInput rawInput
      (Intcode.Finished output _) = Intcode.executeIntcode $ Intcode.Executing 0 0 parsedInput [2] []
  return output
