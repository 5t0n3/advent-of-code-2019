module Day09Boost where

import qualified Intcode

part1 :: IO [Integer]
part1 = do
  rawInput <- readFile "input/day09.txt"
  let testInput = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
      parsedInput = Intcode.parseNumericInput rawInput
      (Intcode.Finished output _) = Intcode.executeIntcode $ Intcode.Executing 0 0 parsedInput [1] []
  return output
