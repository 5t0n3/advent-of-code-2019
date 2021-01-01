module Day05Intcode where

import qualified Intcode

part1 :: IO Integer
part1 = do
  rawInput <- readFile "input/day05.txt"
  let input = Intcode.parseNumericInput rawInput
      program = Intcode.Executing 0 0 input [1] []
      (Intcode.Finished outputs _) = Intcode.executeIntcode program
  return $ last outputs

part2 :: IO Integer
part2 = do
  rawInput <- readFile "input/day05.txt"
  let input = Intcode.parseNumericInput rawInput
      program = Intcode.Executing 0 0 input [5] []
      (Intcode.Finished outputs _) = Intcode.executeIntcode program
  return $ last outputs
