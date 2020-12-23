module Day05Intcode where

import qualified Intcode

part1 :: IO Int
part1 = do
  rawInput <- readFile "input/day05.txt"
  let input = Intcode.parseNumericInput rawInput
      program = Intcode.Executing input [1] []
      (Intcode.Finished outputs _) = Intcode.executeIntcode 0 program
  return $ last outputs

part2 :: IO Int
part2 = do
  rawInput <- readFile "input/day05.txt"
  let input = Intcode.parseNumericInput rawInput
      program = Intcode.Executing input [5] []
      (Intcode.Finished outputs _) = Intcode.executeIntcode 0 program
  return $ last outputs
