module Intcode
  ( executeIntcode,
    stepIntcode,
    parseNumericInput,
    Program (..),
  )
where

import Data.List (genericDrop, genericIndex, genericLength, genericReplicate)
import qualified Utils

data Program
  = Executing Integer Integer [Integer] [Integer] [Integer]
  | Finished [Integer] [Integer]
  | NeedsInput Integer Integer [Integer] [Integer] [Integer]
  | Error Integer [Integer] [Integer] [Integer]
  deriving (Show)

executeIntcode :: Program -> Program
executeIntcode program@(Executing cursor relBase memory inputs outputs) =
  case opCode of
    -- Add
    "01" ->
      executeIntcode $ updateWithResult (sum $ init args)
    -- Multiply
    "02" ->
      executeIntcode $ updateWithResult (product $ init args)
    -- Input
    "03" ->
      case inputs of
        [] -> NeedsInput cursor relBase memory inputs outputs
        current : rest -> executeIntcode $ updateWithResult current
    -- Print
    "04" ->
      executeIntcode (Executing nextCursor relBase memory inputs (head args : outputs))
    -- Jump if true (nonzero)
    "05" ->
      let jumpCursor = if head args == 0 then nextCursor else actualLastArg
       in executeIntcode $ Executing jumpCursor relBase memory inputs outputs
    -- Jump if false (zero)
    "06" ->
      let jumpCursor = if head args == 0 then actualLastArg else nextCursor
       in executeIntcode $ Executing jumpCursor relBase memory inputs outputs
    -- Less than
    "07" ->
      let result = if head args < args !! 1 then 1 else 0
       in executeIntcode $ updateWithResult result
    -- Equal to
    "08" ->
      let result = if head args == args !! 1 then 1 else 0
       in executeIntcode $ updateWithResult result
    -- Adjust relative base
    "09" ->
      executeIntcode $ Executing nextCursor (relBase + head args) memory inputs outputs
    -- Exit (99)
    "99" -> Finished (reverse outputs) memory
    -- Invalid opcode
    _ -> Error cursor memory inputs (reverse outputs)
  where
    (opCode, rawArgs) = parseCodeInfo program
    nextCursor = cursor + toInteger (numOpArgs opCode) + 1
    -- Adjust if position is written to based on mode (0/2); otherwise treat as
    -- a normal argument
    (finalMode, finalArg) = last rawArgs
    actualLastArg =
      if writingOpCode opCode
        then case finalMode of
          Relative -> finalArg + relBase
          Positional -> finalArg
        else fetchArg program finalMode finalArg
    nonImmediateArgs = filter ((/= Immediate) . fst) rawArgs
    -- TODO: Fix to take into account relative arguments as well
    maxAddr =
      maximum $
        map
          ( \(mode, arg) -> case mode of
              Relative -> arg + relBase
              Positional -> arg
          )
          nonImmediateArgs
    updateMemory (Executing cursor relBase _ input output) newMemory = Executing nextCursor relBase newMemory input output
    extendedMemory = memory ++ genericReplicate (maxAddr + 1 - genericLength memory) 0
    extendedMemoryProgram = updateMemory program extendedMemory
    args = map (uncurry (fetchArg extendedMemoryProgram)) rawArgs
    updateWithResult result = updateMemory extendedMemoryProgram $ Utils.replaceNth (fromInteger actualLastArg) result extendedMemory
executeIntcode notExecuting = notExecuting

stepIntcode :: Program -> Program
stepIntcode program@(Executing cursor relBase memory inputs outputs) =
  case opCode of
    -- Add
    "01" ->
      updateWithResult (sum $ init args)
    -- Multiply
    "02" ->
      updateWithResult (product $ init args)
    -- Input
    "03" ->
      case inputs of
        [] -> NeedsInput cursor relBase memory inputs outputs
        current : rest -> updateWithResult current
    -- Print
    "04" ->
      Executing nextCursor relBase memory inputs (head args : outputs)
    -- Jump if true (nonzero)
    "05" ->
      let jumpCursor = if head args == 0 then nextCursor else actualLastArg
       in Executing jumpCursor relBase memory inputs outputs
    -- Jump if false (zero)
    "06" ->
      let jumpCursor = if head args == 0 then actualLastArg else nextCursor
       in Executing jumpCursor relBase memory inputs outputs
    -- Less than
    "07" ->
      let result = if head args < args !! 1 then 1 else 0
       in updateWithResult result
    -- Equal to
    "08" ->
      let result = if head args == args !! 1 then 1 else 0
       in updateWithResult result
    -- Adjust relative base
    "09" ->
      Executing nextCursor (relBase + head args) memory inputs outputs
    -- Exit (99)
    "99" -> Finished (reverse outputs) memory
    -- Invalid opcode
    _ -> Error cursor memory inputs (reverse outputs)
  where
    (opCode, rawArgs) = parseCodeInfo program
    nextCursor = cursor + toInteger (numOpArgs opCode) + 1
    -- Adjust if position is written to based on mode (0/2); otherwise treat as
    -- a normal argument
    (finalMode, finalArg) = last rawArgs
    actualLastArg =
      if writingOpCode opCode
        then case finalMode of
          Relative -> finalArg + relBase
          Positional -> finalArg
        else fetchArg program finalMode finalArg
    nonImmediateArgs = filter ((/= Immediate) . fst) rawArgs
    -- TODO: Fix to take into account relative arguments as well
    maxAddr =
      maximum $
        map
          ( \(mode, arg) -> case mode of
              Relative -> arg + relBase
              Positional -> arg
          )
          nonImmediateArgs
    updateMemory (Executing cursor relBase _ input output) newMemory = Executing nextCursor relBase newMemory input output
    extendedMemory = memory ++ genericReplicate (maxAddr + 1 - genericLength memory) 0
    extendedMemoryProgram = updateMemory program extendedMemory
    args = map (uncurry (fetchArg extendedMemoryProgram)) rawArgs
    updateWithResult result = updateMemory extendedMemoryProgram $ Utils.replaceNth (fromInteger actualLastArg) result extendedMemory
stepIntcode notExecuting = notExecuting

extractOpCode :: String -> String
extractOpCode opCode
  | length opCode < 2 = '0' : opCode
  | otherwise = drop (length opCode - 2) opCode

implicitZeroPad :: Int -> String -> String
implicitZeroPad numArgs opCode
  | length opCode < 2 + numArgs = implicitZeroPad numArgs $ '0' : opCode
  | otherwise = opCode

parseCodeInfo :: Program -> (String, [(ArgMode, Integer)])
parseCodeInfo program@(Executing cursor relBase memory _ _) =
  (opCode, rawArguments)
  where
    (opCode, argInfo) = parseOpCode cursor memory
    argModes = parseArgModes argInfo
    rawArguments = zip argModes . genericDrop (cursor + 1) $ memory

parseOpCode :: Integer -> [Integer] -> (String, String)
parseOpCode cursor memory =
  (opCode, take arguments paddedCode)
  where
    rawCode = show $ memory `genericIndex` cursor
    opCode = extractOpCode rawCode
    arguments = numOpArgs opCode
    paddedCode = implicitZeroPad arguments rawCode

{-
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
resultPos = if last argModes == Relative then originalResultPos + relBase else
originalResultPos
    -}

data ArgMode = Positional | Immediate | Relative deriving (Show, Eq)

parseArgModes :: String -> [ArgMode]
parseArgModes = reverse . map toArgMode
  where
    toArgMode '0' = Positional
    toArgMode '1' = Immediate
    toArgMode '2' = Relative

fetchArg :: Program -> ArgMode -> Integer -> Integer
fetchArg _ Immediate val = val
fetchArg (Executing _ relBase memory _ _) Positional idx = memory `genericIndex` idx
fetchArg (Executing _ relBase memory _ _) Relative relIdx = memory `genericIndex` (relIdx + relBase)

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
