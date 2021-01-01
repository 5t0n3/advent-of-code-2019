module Intcode
  ( executeIntcode,
    parseNumericInput,
    Program (..),
  )
where

import qualified Utils

data Program
  = Executing Int [Int] [Int] [Int]
  | Finished [Int] [Int]
  | NeedsInput Int [Int] [Int] [Int]
  | Error Int [Int] [Int] [Int]
  deriving (Show)

executeIntcode :: Program -> Program
executeIntcode (Executing cursor opCodes inputs outputs) =
  case justOpCode of
    -- Add
    "01" ->
      executeIntcode (updateWithResult nextCursor (sum opArgs) inputs outputs)
    -- Multiply
    "02" ->
      executeIntcode (updateWithResult nextCursor (product opArgs) inputs outputs)
    -- Input
    "03" ->
      case inputs of
        [] -> NeedsInput cursor opCodes inputs outputs
        current : rest -> executeIntcode (updateWithResult nextCursor current rest outputs)
    -- Print
    "04" ->
      executeIntcode (Executing nextCursor opCodes inputs (head opArgs : outputs))
    -- Jump if true (nonzero)
    "05" ->
      let jumpCursor = if head opArgs == 0 then nextCursor else last opArgs
       in executeIntcode $ Executing jumpCursor opCodes inputs outputs
    -- Jump if false (zero)
    "06" ->
      let jumpCursor = if head opArgs == 0 then last opArgs else nextCursor
       in executeIntcode $ Executing jumpCursor opCodes inputs outputs
    -- Less than
    "07" ->
      let result = if head opArgs < last opArgs then 1 else 0
       in executeIntcode (updateWithResult nextCursor result inputs outputs)
    -- Equal to
    "08" ->
      let result = if head opArgs == last opArgs then 1 else 0
       in executeIntcode (updateWithResult nextCursor result inputs outputs)
    -- Exit (99)
    "99" -> Finished (reverse outputs) opCodes
    -- Invalid opcode
    _ -> Error cursor opCodes inputs (reverse outputs)
  where
    rawOpCode = show $ opCodes !! cursor
    justOpCode = extractOpCode rawOpCode -- Just last 2 digits
    numArgs = numOpArgs justOpCode
    paddedOpCode = implicitZeroPad numArgs rawOpCode
    nextCursor = cursor + numArgs + 1
    (opArgs, resultPos) = parseOpCodeArgs cursor opCodes paddedOpCode numArgs
    updateWithResult cursor result = Executing cursor $ Utils.replaceNth resultPos result opCodes
executeIntcode notExecuting = notExecuting

extractOpCode :: String -> String
extractOpCode opCode
  | length opCode < 2 = '0' : opCode
  | otherwise = drop (length opCode - 2) opCode

implicitZeroPad :: Int -> String -> String
implicitZeroPad numArgs opCode
  | length opCode < 2 + numArgs = implicitZeroPad numArgs $ '0' : opCode
  | otherwise = opCode

parseOpCodeArgs :: Int -> [Int] -> String -> Int -> ([Int], Int)
parseOpCodeArgs cursor fullList fullCodeStr numArgs =
  if writingOpCode (extractOpCode fullCodeStr)
    then (init argVals, last codeArgs) -- Don't include result position
    else (argVals, last codeArgs)
  where
    (_ : paramTail) = drop cursor fullList
    codeArgs = take numArgs paramTail
    argModes = parseArgModes fullCodeStr numArgs
    argVals = fetchArgs fullList codeArgs argModes

data ArgMode = Immediate | Positional deriving (Show, Eq)

parseArgModes :: String -> Int -> [ArgMode]
parseArgModes codeStr numArgs =
  map toArgMode argModes
  where
    argModes = reverse . take numArgs $ codeStr
    toArgMode '0' = Positional
    toArgMode '1' = Immediate

fetchArgs :: [Int] -> [Int] -> [ArgMode] -> [Int]
fetchArgs fullList = zipWith fetchArg
  where
    fetchArg val Immediate = val
    fetchArg idx Positional = fullList !! idx

numOpArgs :: String -> Int
numOpArgs code
  | code `elem` ["01", "02", "07", "08"] = 3
  | code `elem` ["03", "04"] = 1
  | code `elem` ["05", "06"] = 2
  | code == "99" = 0

writingOpCode :: String -> Bool
writingOpCode = flip elem ["01", "02", "03", "07", "08"]

parseNumericInput :: String -> [Int]
parseNumericInput [] = []
parseNumericInput rawInput = map read . Utils.splitAtCommas $ rawInput
