module Day02Intcode where

import qualified Intcode
import qualified Utils

part1 :: IO Integer
part1 = do
  rawInput <- readFile "input/day02.txt"
  let originalInput = Intcode.parseNumericInput rawInput
      updatedInput = replaceNounVerb 12 2 originalInput
      program = Intcode.Executing 0 0 updatedInput [] []
      (Intcode.Finished _ result) = Intcode.executeIntcode program
  return $ head result

part2 :: IO Integer
part2 = do
  rawInput <- readFile "input/day02.txt"
  let originalInput = Intcode.parseNumericInput rawInput
      resLists = testNounVerbRange [0 .. 99] originalInput
  let [_, noun, verb] = head resLists
  return $ 100 * noun + verb

testNounVerbRange :: [Integer] -> [Integer] -> [[Integer]]
testNounVerbRange nvRange valueList =
  [ take 3 result
    | noun <- nvRange,
      verb <- nvRange,
      let updatedList = replaceNounVerb noun verb valueList
          program = Intcode.Executing 0 0 updatedList [] []
          (Intcode.Finished _ result) = Intcode.executeIntcode program,
      head result == 19690720
  ]

replaceNounVerb :: Integer -> Integer -> [Integer] -> [Integer]
replaceNounVerb noun verb fullList =
  Utils.replaceNth 1 noun (Utils.replaceNth 2 verb fullList)
