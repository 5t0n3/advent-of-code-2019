module Day2Intcode where

import qualified Intcode
import qualified Utils

part1 :: IO Int
part1 = do
  rawInput <- readFile "input/day2input.txt"
  let originalInput = Intcode.parseNumericInput rawInput
  let updatedInput = replaceNounVerb 12 2 originalInput
  result <- Intcode.executeIntcodeList 0 updatedInput
  return $ head result

part2 :: IO Int
part2 = do
  rawInput <- readFile "input/day2input.txt"
  let originalInput = Intcode.parseNumericInput rawInput
  let (solNoun,solVerb) = head . testOpCodes [0..99] $ originalInput
  return (100 * solNoun + solVerb)

testOpCodes :: [Int] -> [Int] -> [(Int, Int)]
testOpCodes nvRange valueList = do
  [(noun, verb) |
    noun <- nvRange,
    verb <- nvRange,
    let updatedList = replaceNounVerb noun verb valueList
        result = Intcode.executeIntcodeListNoIO 0 updatedList
    in head result == 19690720]

replaceNounVerb :: Int -> Int -> [Int] -> [Int]
replaceNounVerb noun verb fullList =
  Utils.replaceNth 1 noun (Utils.replaceNth 2 verb fullList)
