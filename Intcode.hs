module Intcode
  ( executeIntcode,
    parseNumericInput,
    Program (..),
  )
where

import qualified Utils

data Program = Executing [Int] [Int] [Int] | Finished [Int] [Int] | Error Int [Int] deriving (Show)

executeIntcode :: Int -> Program -> Program
executeIntcode cursor program@(Executing opCodes inputs outputs) =
  case last fullOpCode of
    -- Add
    '1' ->
      executeIntcode nextCursor (updateWithResult (sum opArgs) inputs outputs)
    -- Multiply
    '2' ->
      executeIntcode nextCursor (updateWithResult (product opArgs) inputs outputs)
    -- Input
    '3' ->
      case inputs of
        [] -> Error cursor opCodes
        current : rest -> executeIntcode nextCursor (updateWithResult current rest outputs)
    -- Print
    '4' ->
      executeIntcode nextCursor (Executing opCodes inputs (head opArgs : outputs))
    -- Jump if true (nonzero)
    '5' ->
      let jumpCursor = if head opArgs == 0 then nextCursor else last opArgs
       in executeIntcode jumpCursor program
    -- Jump if false (zero)
    '6' ->
      let jumpCursor = if head opArgs == 0 then last opArgs else nextCursor
       in executeIntcode jumpCursor program
    -- Less than
    '7' ->
      let result = if head opArgs < last opArgs then 1 else 0
       in executeIntcode nextCursor (updateWithResult result inputs outputs)
    -- Equal to
    '8' ->
      let result = if head opArgs == last opArgs then 1 else 0
       in executeIntcode nextCursor (updateWithResult result inputs outputs)
    -- Exit (99)
    '9' -> Finished (reverse outputs) opCodes
    -- Invalid opcode
    _ -> Error cursor opCodes
  where
    fullOpCode = show $ opCodes !! cursor
    numArgs = numOpArgs (read [last fullOpCode])
    nextCursor = cursor + numArgs + 1
    (opArgs, resultPos) = parseOpCodeArgs cursor opCodes fullOpCode numArgs
    updateWithResult result = Executing $ Utils.replaceNth resultPos result opCodes

parseOpCodeArgs :: Int -> [Int] -> String -> Int -> ([Int], Int)
parseOpCodeArgs cursor fullList fullCodeStr numArgs =
  (argVals, last codeArgs)
  where
    (_ : paramTail) = drop cursor fullList
    codeArgs = take numArgs paramTail
    argModes = parseArgModes fullCodeStr numArgs
    argVals = fetchArgs fullList codeArgs argModes

data ArgMode = Immediate | Positional deriving (Show, Eq)

parseArgModes :: String -> Int -> [ArgMode]
parseArgModes codeStr numArgs =
  tail . reverse $ implicitPositional ++ givenModes
  where
    givenModes = map (\c -> if c == '1' then Immediate else Positional) (init codeStr)
    writePosConstant = if last codeStr `elem` "1278" then 0 else 1
    implicitPositional = replicate (numArgs - length givenModes + writePosConstant) Positional

fetchArgs :: [Int] -> [Int] -> [ArgMode] -> [Int]
fetchArgs fullList paramList argModes =
  foldr fetchArg [] (zip paramList argModes)
  where
    fetchArg = \(val, mode) acc -> (if mode == Immediate then val else fullList !! val) : acc

numOpArgs :: Int -> Int
numOpArgs code
  | code `elem` [1, 2, 7, 8] = 3
  | code `elem` [3, 4] = 1
  | code `elem` [5, 6] = 2
  | code == 99 = 0
  | otherwise = -1 -- This shouldn't happen

parseNumericInput :: String -> [Int]
parseNumericInput [] = []
parseNumericInput rawInput = map read . Utils.splitAtCommas $ rawInput
