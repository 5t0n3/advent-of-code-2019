module Day5Intcode where

import qualified Intcode

main :: IO [Int]
main = do
  rawInput <- readFile "input/day5input.txt"
  let input = Intcode.parseNumericInput rawInput
  Intcode.executeIntcodeList 0 input
