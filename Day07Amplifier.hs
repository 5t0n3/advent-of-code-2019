module Day07Amplifier where

import Data.List (nub)
import qualified Intcode

part1 :: IO Int
part1 = do
  rawInput <- readFile "input/day07.txt"
  let parsedIntcode = Intcode.parseNumericInput rawInput
      phaseSequences = filter ((== 5) . length . nub) . mapM (const [0 .. 4]) $ [1 .. 5]
      allAmpResults = map (testAmpSequence parsedIntcode) phaseSequences
  return $ maximum allAmpResults

testAmpSequence :: [Int] -> [Int] -> Int
testAmpSequence baseProgram phaseSequence =
  let (Intcode.Finished outputA _) = Intcode.executeIntcode 0 $ Intcode.Executing baseProgram [phaseA, 0] []
      (Intcode.Finished outputB _) = Intcode.executeIntcode 0 $ Intcode.Executing baseProgram [phaseB, head outputA] []
      (Intcode.Finished outputC _) = Intcode.executeIntcode 0 $ Intcode.Executing baseProgram [phaseC, head outputB] []
      (Intcode.Finished outputD _) = Intcode.executeIntcode 0 $ Intcode.Executing baseProgram [phaseD, head outputC] []
      (Intcode.Finished outputE _) = Intcode.executeIntcode 0 $ Intcode.Executing baseProgram [phaseE, head outputD] []
   in head outputE
  where
    [phaseA, phaseB, phaseC, phaseD, phaseE] = phaseSequence