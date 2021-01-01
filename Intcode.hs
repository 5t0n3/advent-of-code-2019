module Intcode
  ( executeIntcode,
    parseNumericInput,
    Program (..),
  )
where

import Data.List (genericIndex)
import qualified Utils

data Program
  = Executing Integer Integer [Integer] [Integer] [Integer]
  | Finished [Integer] [Integer]
  | NeedsInput Integer Integer [Integer] [Integer] [Integer]
  | Error Integer [Integer] [Integer] [Integer]
  deriving (Show)

executeIntcode :: Program -> Program
executeIntcode (Executing cursor relBase opCodes inputs outputs) =
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
        [] -> NeedsInput cursor relBase opCodes inputs outputs
        current : rest -> executeIntcode (updateWithResult nextCursor current rest outputs)
    -- Print
    "04" ->
      executeIntcode (Executing nextCursor relBase opCodes inputs (head opArgs : outputs))
    -- Jump if true (nonzero)
    "05" ->
      let jumpCursor = if head opArgs == 0 then nextCursor else fromInteger $ last opArgs
       in executeIntcode $ Executing jumpCursor relBase opCodes inputs outputs
    -- Jump if false (zero)
    "06" ->
      let jumpCursor = if head opArgs == 0 then fromInteger $ last opArgs else nextCursor
       in executeIntcode $ Executing jumpCursor relBase opCodes inputs outputs
    -- Less than
    "07" ->
      let result = if head opArgs < last opArgs then 1 else 0
       in executeIntcode (updateWithResult nextCursor result inputs outputs)
    -- Equal to
    "08" ->
      let result = if head opArgs == last opArgs then 1 else 0
       in executeIntcode (updateWithResult nextCursor result inputs outputs)
    -- Adjust relative base
    "09" ->
      executeIntcode $ Executing nextCursor (relBase + fromInteger (head opArgs)) opCodes inputs outputs
    -- Exit (99)
    "99" -> Finished (reverse outputs) opCodes
    -- Invalid opcode
    _ -> Error cursor opCodes inputs (reverse outputs)
  where
    rawOpCode = show $ opCodes `genericIndex` cursor
    justOpCode = extractOpCode rawOpCode -- Just last 2 digits
    numArgs = numOpArgs justOpCode
    paddedOpCode = implicitZeroPad numArgs rawOpCode
    nextCursor = cursor + toInteger numArgs + 1
    argModes = parseArgModes paddedOpCode numArgs
    readWritePos =
      case last argModes of
        Relative -> fromInteger (opCodes `genericIndex` nextCursor - 1) + relBase
        _ -> fromInteger (opCodes `genericIndex` nextCursor - 1)
    extendedMemory = if readWritePos >= toInteger (length opCodes) then opCodes ++ replicate (fromInteger readWritePos - length opCodes) 0 else opCodes
    (opArgs, resultPos) = parseOpCodeArgs cursor relBase extendedMemory paddedOpCode numArgs
    updateWithResult cursor result = Executing cursor relBase $ Utils.replaceNth (fromInteger resultPos) result extendedMemory
executeIntcode notExecuting = notExecuting

extractOpCode :: String -> String
extractOpCode opCode
  | length opCode < 2 = '0' : opCode
  | otherwise = drop (length opCode - 2) opCode

implicitZeroPad :: Int -> String -> String
implicitZeroPad numArgs opCode
  | length opCode < 2 + numArgs = implicitZeroPad numArgs $ '0' : opCode
  | otherwise = opCode

parseOpCodeArgs :: Integer -> Integer -> [Integer] -> String -> Int -> ([Integer], Integer)
parseOpCodeArgs cursor relBase fullList fullCodeStr numArgs =
  if writingOpCode (extractOpCode fullCodeStr)
    then (init argVals, last codeArgs) -- Don't include result position
    else (argVals, last codeArgs)
  where
    (_ : paramTail) = drop (fromInteger cursor) fullList
    codeArgs = take numArgs paramTail
    argModes = parseArgModes fullCodeStr numArgs
    argVals = fetchArgs fullList relBase codeArgs argModes
    originalResultPos = last codeArgs
    resultPos = if last argModes == Relative then originalResultPos + relBase else originalResultPos

data ArgMode = Positional | Immediate | Relative deriving (Show, Eq)

parseArgModes :: String -> Int -> [ArgMode]
parseArgModes codeStr numArgs =
  map toArgMode argModes
  where
    argModes = reverse . take numArgs $ codeStr
    toArgMode '0' = Positional
    toArgMode '1' = Immediate
    toArgMode '2' = Relative

fetchArgs :: [Integer] -> Integer -> [Integer] -> [ArgMode] -> [Integer]
fetchArgs fullList relBase = zipWith fetchArg
  where
    fetchArg val Immediate = val
    fetchArg idx Positional = fullList `genericIndex` idx
    fetchArg relIdx Relative = fullList `genericIndex` (relIdx + relBase)

numOpArgs :: String -> Int
numOpArgs code
  | code `elem` ["01", "02", "07", "08"] = 3
  | code `elem` ["03", "04", "09"] = 1
  | code `elem` ["05", "06"] = 2
  | code == "99" = 0

writingOpCode :: String -> Bool
writingOpCode = flip elem ["01", "02", "03", "07", "08"]

parseNumericInput :: String -> [Integer]
parseNumericInput [] = []
parseNumericInput rawInput = map read . Utils.splitAtCommas $ rawInput
