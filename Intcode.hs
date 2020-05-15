module Intcode
  ( executeIntcodeList
  , executeIntcodeListNoIO
  , parseNumericInput
  ) where

import qualified Utils

executeIntcodeList :: Int -> [Int] -> IO [Int]
executeIntcodeList cursor inputList =
  case last fullOpCode of
    -- Add 
    '1' ->
      let result = foldl1 (+) opArgs
      in return (Utils.replaceNth resultPos result inputList) >>= executeIntcodeList (cursor + numArgs + 1)
    -- Multiply
    '2' ->
      let result = foldl1 (*) opArgs
      in return (Utils.replaceNth resultPos result inputList) >>= executeIntcodeList (cursor + numArgs + 1)
    -- Input
    '3' -> do
      result <- getLine
      return (Utils.replaceNth resultPos (read result) inputList) >>= executeIntcodeList (cursor + numArgs + 1)
    -- Print
    '4' -> do
      print (head opArgs)
      return inputList >>= executeIntcodeList (cursor + numArgs + 1)
    -- Jump if true (nonzero)
    '5' ->
      let newCursor = if head opArgs == 0 then cursor + numArgs + 1 else last opArgs
      in return inputList >>= executeIntcodeList newCursor
    -- Jump if false (zero)
    '6' ->
      let newCursor = if head opArgs == 0 then last opArgs else cursor + numArgs + 1
      in return inputList >>= executeIntcodeList newCursor
    -- Less than
    '7' ->
      let result = if head opArgs < last opArgs then 1 else 0
      in return (Utils.replaceNth resultPos result inputList) >>= executeIntcodeList (cursor + numArgs + 1)
    -- Equal to
    '8' ->
      let result = if head opArgs == last opArgs then 1 else 0
      in return (Utils.replaceNth resultPos result inputList) >>= executeIntcodeList (cursor + numArgs + 1)
    -- Invalid opcode/99 (exit)
    _ -> return inputList
  where fullOpCode = show $ inputList !! cursor
        numArgs = numOpArgs (read [last fullOpCode])
        (opArgs,resultPos) = parseOpCodeArgs cursor inputList fullOpCode numArgs

executeIntcodeListNoIO :: Int -> [Int] -> [Int]
executeIntcodeListNoIO cursor inputList =
  case last fullOpCode of
    -- Add 
    '1' ->
      let result = foldl1 (+) opArgs
      in executeIntcodeListNoIO (cursor + numArgs + 1) (Utils.replaceNth resultPos result inputList)
    -- Multiply
    '2' ->
      let result = foldl1 (*) opArgs
      in executeIntcodeListNoIO (cursor + numArgs + 1) (Utils.replaceNth resultPos result inputList)
    -- Jump if true (nonzero)
    '5' ->
      let newCursor = if head opArgs == 0 then cursor + numArgs + 1 else last opArgs
      in executeIntcodeListNoIO newCursor inputList
    -- Jump if false (zero)
    '6' ->
      let newCursor = if head opArgs == 0 then last opArgs else cursor + numArgs + 1
      in executeIntcodeListNoIO newCursor inputList
    -- Less than
    '7' ->
      let result = if head opArgs < last opArgs then 1 else 0
      in executeIntcodeListNoIO (cursor + numArgs + 1) (Utils.replaceNth resultPos result inputList)
    -- Equal to
    '8' ->
      let result = if head opArgs == last opArgs then 1 else 0
      in executeIntcodeListNoIO (cursor + numArgs + 1) (Utils.replaceNth resultPos result inputList)
    -- Invalid opcode/99 (exit)
    _ -> inputList
  where fullOpCode = show $ inputList !! cursor
        numArgs = numOpArgs (read [last fullOpCode])
        (opArgs,resultPos) = parseOpCodeArgs cursor inputList fullOpCode numArgs
        
parseOpCodeArgs :: Int -> [Int] -> String -> Int -> ([Int], Int)
parseOpCodeArgs cursor fullList fullCodeStr numArgs =
  (argVals,last codeArgs)
  where (_:paramTail) = snd . splitAt cursor $ fullList
        codeArgs = take numArgs paramTail
        argModes = parseArgModes fullCodeStr numArgs
        argVals = fetchArgs fullList codeArgs argModes

data ArgMode = Immediate | Positional deriving (Show, Eq)

parseArgModes :: String -> Int -> [ArgMode]
parseArgModes codeStr numArgs =
  tail . reverse $ implicitPositional ++ givenModes
  where givenModes = map (\c -> if c == '1' then Immediate else Positional) (init codeStr)
        writePosConstant = if elem (last codeStr) "1278" then 0 else 1
        implicitPositional = replicate (numArgs - length givenModes + writePosConstant) Positional

fetchArgs :: [Int] -> [Int] -> [ArgMode] -> [Int]
fetchArgs fullList paramList argModes =
  foldr fetchArg [] (zip paramList argModes)
  where fetchArg = \(val,mode) acc -> (if mode == Immediate then val else (fullList !! val)) : acc

numOpArgs :: Int -> Int
numOpArgs code
  | elem code [1,2,7,8] = 3
  | elem code [3,4] = 1
  | elem code [5,6] = 2
  | code == 99 = 0
  | otherwise = -1 -- This shouldn't happen
   
parseNumericInput :: String -> [Int]
parseNumericInput [] = []
parseNumericInput rawInput = map read . Utils.splitAtCommas $ rawInput

