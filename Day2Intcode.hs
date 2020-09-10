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
  resLists <- testOpCodes [0..99] originalInput
  let (_:noun:verb:_) = head . filter (\(res:_) -> res == 19690720) $ resLists
  return $ 100 * noun + verb
  
testOpCodes :: [Int] -> [Int] -> IO [[Int]]
testOpCodes nvRange valueList =
  sequence [Intcode.executeIntcodeList 0 updatedList |
             noun <- nvRange,
             verb <- nvRange,
             let updatedList = replaceNounVerb noun verb valueList]

replaceNounVerb :: Int -> Int -> [Int] -> [Int]
replaceNounVerb noun verb fullList =
  Utils.replaceNth 1 noun (Utils.replaceNth 2 verb fullList)
