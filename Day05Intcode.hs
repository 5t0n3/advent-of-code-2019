module Day05Intcode where

import qualified Intcode

main :: IO [Int]
main = do
  rawInput <- readFile "input/day05.txt"
  let input = Intcode.parseNumericInput rawInput
  Intcode.executeIntcodeList 0 input
