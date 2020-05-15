import Intcode

main :: IO [Int]
main = do
  rawInput <- readFile "day5input.txt"
  let input = inputToList rawInput
  executeIntcodeList 0 input
